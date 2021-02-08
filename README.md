# Living a Life of Labor in Bolivia

This is the repo for the Bolivia Life of Labor shiny app, created by Rui Su and Carla Cristina Solis Uehara, members of the GFDRR analytics team.
We are indebted to our colleagues--Brian Walsh, Onimi Jademi, Bramka Jafino, and everyone else on the social resilience team--for their helpful advice and continuous camaraderie.
For feedback, collaborations, and questions, please contact <rsu@worldbank.org> or <csolisuehara@worldbank.org>.

## Background
The Life of Labor project germinated as an inquiry into the dynamics of gender and labor in Bolivia.
In particular, we hoped to explore women's work conditions:
why so many of them were not getting paid for their labor, and what other disparities existed between women and men.
Before we realized, our initial probe flourished into something much more ambitious:
to investigate the intersecting socioeconomic structures that shaped countless people's life trajectories in the Bolivian society,
including but not limited to gender, geography, disability, indigenous identity, etc.
Ultimately, this project not only provides a data-informed perspective of various hierarchies and challenges in Bolivia,
but also serves as an example of how complex social issues could be disentangled, represented, and communicated through data,
while respecting the rich lived experiences of everyday Bolivians.

## Methodology
We use the publicly available 2018 Bolivia household survey (Encuesta de Hogares) data as a basis for our analysis.
Operating on the belief that social policies influence people differently at different points in their lives
(e.g. educational policies affect children and youth much more than older adults),
we took a lifecycle approach and segmented the population into four age groups:
children under 18, youth 18-24, working-age adults 25-60, and older adults over 60.
For each age group, we focus on different life decisions (represented as target variables) and identify the specific socioeconomic characteristics that result in unequal outcomes.
For example, for children, we examine the aspects of education and employment, since child workers have long been a controversial issue in Bolivia.
Therefore, our two initial target variables are 1) whether a child is in school and 2) whether they are working.
The full list of target variables are below:  

**Children under 18**: in school, employed  
**Youth 18-24**: in school, employed  
**Adults 25-60**: employed, paid, NEET (Not in Education, Employment, or Training)  
**Older adults 60+**: employed, paid  

After selecting the target variables, we run random forest models to generate the variable importance list,
some of which are plotted in the shiny app, and distill the most important socioeconomic predictors for further exploration.
However, when the random forest model yields a low AUC/balanced accuracy--usually due to a high level of noise in the dataset--we
avoid using the variable importance results and instead opt for simpler descriptive statistics.

## Ethics
On the most basic level, research ethics require anonymity--the survey data are anonymous, and all names used in the shiny app are fictional.
However, we would be remiss if we do not move beyond this basic level of care and consider our responsibilities, as researchers and World Bank employees,
to the people and communities to whom we owe the data.
To be able to represent someone is to have power over someone, and in a time when increasing amounts of data as a form of capital
are aggregated, controlled, and parsed, we ought to consider how we exercise our power to center marginalized voices and provoke transformative actions.
In this shiny app, we seek to balance big-picture conclusions with detailed individual stories of those traditionally distant from the center of power,
such as child workers.
More important, we contemplate not just what the conclusions are, but also what deep-rooted injustices they reveal.
Thus, beyond pointing out which socioeconomic characteristics are most connected to present inequalities,
we weave together the otherwise disjointed conclusions to paint a picture of cross-cutting hierarchies that persistently hurt some people's opportunities toward dignity and prosperity.
By doing so, we hope to shed light on the social, cultural, and institutional factors at play that fundamentally define aspects of the Bolivian society.  

Finally, we believe that achieving fair and just representation is a long, iterative process,
and our efforts thus far are by no means comprehensive or complete.
We have, however briefly, inhabited Bolivia's data-land, before having listened to enough stories and thoughts of the real humans living in Bolivia's web of life.
Therefore, we invite your inputs and collaboration in holding us accountable to the people whose stories we try to tell and filling our blindspots as non-Bolivians.

## Vision
This project is currently under development.
We will continue to fill in the remaining pages in the shiny app and make small adjustments.
In the future, we envision this type of analytics being broadly applicable to fields like social protection policy targeting,
social inclusion, monitoring & evaluation, etc.  

To take a step back, we see our work as part of the obra of a long lineage of contributions made by researchers and policymakers dedicated to equity and justice globally.
We hope our contribution, however small, serves as a demonstration of how data could be mobilized to elevate grassroots stories
using relatively simple statistical methods and visualization techniques.
This is our attempt to excavate data's potential as a bridge to better democracy.
