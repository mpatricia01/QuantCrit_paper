################################################################################
##
## [ PROJ ] < ASHE paper>
## [ FILE ] < tables_figures.R>
## [ AUTH ] < Patricia Martín/@mpatricia01>
## [ INIT ] < 10/26/22 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
#p_load() function will check if libraries are installed, if not, will install them and load them
#p_load() function is a function in the pacman package
pacman::p_load(tidyverse, haven, labelled, epiDisplay, sjPlot, lubridate, readxl, gt, RColorBrewer, openxlsx, hrbrthemes, patchwork, treemap, sunburstR, viridis, wordcloud2, moonBook, webr)

## ---------------------------
## directory paths
## ---------------------------
#file.path() function will print out a string or a file path based on elements given separated by quotations and a comma
data_dir <- file.path('.', 'data') #"./data" output


## ---------------------------
## Read in analysis spreadsheet
## ---------------------------
analysis <- read_csv(str_c(data_dir, "/QuantCrit_analysis_matrix.csv"))

#create var to get a running count of # of articles
#create column with 1
analysis$rec <- 1

yr_type <- analysis %>%
  group_by(Year) %>%
  mutate(y_label = n(),
         count_yr= cumsum(rec)) %>%
  dplyr::select(Year, Terminology_Type, y_label, count_yr)

yr_type2 <- yr_type %>%
  ungroup() %>%
  group_by(Terminology_Type, Year) %>%
  dplyr::summarise(count_yr_type = n(), .groups = 'drop') %>%
  arrange(Year, Terminology_Type)



#plot quantcrit and critical quant articles by year


#EDIT THIS FIGURE
yr_type2 %>% filter(Year != 1989 & !is.na(Terminology_Type)) %>% #remove NAs for now
  ggplot() + 
  geom_col(aes(x = Year, y= count_yr_type, fill=Terminology_Type), width = 0.5) + 
  #geom_text(aes(x = Year, y= count_yr_type, label = count_yr_type)) + 
  #theme_minimal() + 
  #theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  scale_x_continuous(breaks=seq(2000, 2022, 1)) + 
  labs(title="Critical Quantitative articles published from 2000-2022", 
       subtitle="Articles & Book Chapters published across critical quantitative paradigm",
       y = "# of publications") +
  scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3"), 
                    breaks = c("Critical Quant", "QuantCrit", "Other"),
                    name="Critical Quantitative\nParadigm")



#data investigations about articles and years published
analysis %>%
  filter(Year==2007) %>%
  dplyr::select(Citation, Journal)


# EDIT THIS FIG
p <- analysis %>% filter(!is.na(Terminology_Type)) %>% #remove NAs for now
  ggplot(aes(x=Article_Type))

p + geom_bar(aes(fill=Terminology_Type), width = 0.5) + 
  #theme_minimal() +
  scale_y_continuous(breaks=seq(0, 44, 2)) +
  #theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Articles published by scholarly type", 
       subtitle="# of Publications across critical quantitative paradigms",
       x = "",
       y = "# of publications") +
  scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3"), 
                    breaks = c("Critical Quant", "QuantCrit", "Other"),
                    name="Critical Quantitative\nParadigm") +
  coord_flip()


analysis %>%
  group_by(Terminology_Type) %>%
  count(Article_Type) 
#table for article type
#graph for years published by quantcrit or critquant
#wordcloud of journal articles

analysis %>%
  count(Journal)

#create a df for count of journals
journals <- analysis %>%
  group_by(Journal) %>%
  summarise(Value = n()) 

#create lollipop plot
journals %>%
  filter(!is.na(Value) & !startsWith(Journal, "Book") & Journal != "Dissertation") %>%
  arrange(Value) %>%
  #tail(20) %>%
  mutate(Journal=factor(Journal, Journal)) %>%
  ggplot( aes(x=Journal, y=Value) ) +
  geom_segment( aes(x=Journal ,xend=Journal, y=0, yend=Value), color="grey") +
  geom_point(size=3, color="#1B9E77") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Count of Journals publishing articles")


#create treemap with Journal article counts

treemap(journals,
        
        # data
        index="Journal",
        vSize="Value",
        type="index",
        
        # Main
        title="",
        palette="Accent",
        
        # Borders:
        border.col=c("black"),             
        border.lwds=1,                         
        
        # Labels
        fontsize.labels=0.5,
        fontcolor.labels="black",
        fontface.labels=1,            
        bg.labels=c("transparent"),              
        align.labels=c("left", "top"),                                  
        overlap.labels=0.5,
        force.print.labels = T,
        inflate.labels=T# If true, labels are bigger when rectangle is bigger.
        
)

#sunburst plot

#remove NAs, Books, & Diss; create 0/1 dummy variable of ed specific journal
journals <- journals %>%
  filter(!is.na(Value) & !startsWith(Journal, "Book") & Journal != "Dissertation") %>%
  mutate(ed = ifelse(Journal %in% c("The Professional Geographer", "Race, Gender, and Class",
                                    "Annals of American Association of Geographers", "Applied Geography",
                                    "Critical Public Health", "Digital Societies", "Environment and Planning A",
                                    "Frontiers in Big Data", "International Journal of Sexual Health",
                                    "Journal of Family Theory and Review", "Journal of Poverty",
                                    "Journal of Racial and Ethnic Health Disparities", "Social Science Research",
                                    "The Journal of Pan African Studies"),0,1)) %>%
  arrange(desc(Value))
  
p <- sunburst(journals, legend=F)
p


#create simple descriptive table
#calculate % journals publishing work
journals <- journals %>%
  mutate(Percent = round(Value/sum(Value)*100,2))

#create table for all ED specific journals & journals in other disciplines
journals %>% filter(ed==1) %>% dplyr::select(!ed) %>% gt()
journals %>% filter(ed==0) %>% dplyr::select(!ed) %>% gt()



#create discipline categories
journals <- journals %>%
  mutate(discipline = case_when(
    ed == 1 ~ "Education",
    Journal %in% c("The Professional Geographer", "Applied Geography") ~ "Geography",
    Journal %in% c("Digital Societies", "Frontiers in Big Data") ~ "Digital Studies & Big Data",
    Journal %in% c("Critical Public Health", "Journal of Racial and Ethnic Health Disparities", "International Journal of Sexual Health") ~ "Public Health",
    Journal %in% c("Journal of Poverty", "Social Science Research", "The Journal of Pan African Studies", "Race, Gender, and Class") ~ "Race, Gender, & Social Sciences",
    Journal == "Environment and Planning A" ~ "Urban Planning",
    Journal == "Journal of Family Theory and Review" ~ "Family Theory"
  ))

disciplines <- journals %>%
  group_by(discipline) %>%
  summarise(n_per_group = sum(Value))


#figs for disciplines

#table
disciplines %>% arrange(desc(n_per_group)) %>% gt() %>%
  cols_label(
    discipline = md("**Journal Discipline**"),
    n_per_group = md("**Num. of articles**")
  )

#lollipop fig
disciplines %>%
  arrange(n_per_group) %>%
  #tail(20) %>%
  mutate(Discipline=factor(discipline, discipline)) %>%
  ggplot( aes(x=Discipline, y=n_per_group) ) +
  geom_segment( aes(x=Discipline ,xend=Discipline, y=0, yend=n_per_group), color="grey") +
  geom_point(size=3, color="#1B9E77") +
  geom_text(aes(x=Discipline, y = n_per_group, label = n_per_group), colour = "black", nudge_x = 0.4, nudge_y = 0.2) + 
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Number of articles by Journal discipline")



#create a word cloud of journals

wordcloud2(journals,  minRotation = -pi/2, maxRotation = -pi/2,
           backgroundColor = "white", color="#69b3a2")



#some investigations
#list all articles published from Race Ethnicity and Education journal and look at citation
View(analysis %>%
  dplyr::select(Journal, Citation, Year) %>%
  filter(Journal=="Race Ethnicity and Education"))

View(analysis %>%
       dplyr::select(Journal, Citation, Year) %>%
       filter(Journal=="New Directions for Institutional Research"))


#edit methods section
#work on figures 
