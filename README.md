# Criminal Map under Bayesian Framework

### Motivation

Before I moved to the U.S., my mom often warned me, "It’s different from Taiwan. Don’t go to dangerous places." I didn't take it seriously, thinking, “Come on! That won’t happen to me.” I didn’t believe it until I experienced it myself.

<p align="center">
  <img src="https://github.com/OuOLeaf/Bayesian-Crime-Map/blob/master/readme-photo/EyeBall.png?raw=true" width="450" height="300"/>
</p>

During a break in a busy semester, I went on a road trip to Dallas to visit the Giant Eyeball downtown. After parking near the spot, a strange man started yelling at me, and I noticed someone moving oddly in the shadows. That’s when I realized the danger. Why hadn’t I prepared for this?

After that trip, I decided to learn more about where I live—specifically, which areas in Houston are relatively dangerous—and embarked on this project.

### Method

I collected 2023-2024 crime data from the Houston Police Department and filtered out crimes we want to avoid, like assault and arson. Initially, I planned to evaluate safety by the total number of crimes but realized this method was biased due to varying region sizes. To address this, I incorporated population data to calculate crime rates.

The Poisson-Gamma model is a Bayesian approach commonly used for count data. In this case, monthly average crime counts follow a Poisson distribution. This model helps estimate the distribution of average crime cases over time.

A Bayesian model adapts like memory, recalling its prior knowledge and updating it with new data. I used 2021–2022 crime data as a prior and updated it with recent data from 2023–2024.

### Result

<p align="center"><img src="https://github.com/OuOLeaf/Bayesian-Crime-Map/blob/master/readme-photo/Heatmap.png?raw=true" width="600" height="450"/></p>

Let’s see the result. In Houston City, the most dangerous places are South Central, Central to Westside, followed by northern regions, then southern regions. With this map, next time somebody plans to visit or rent a place in Houston, I can share my results to help them make informed decisions. Plus, I connected with a friend, Melisa, who works in the police station thanks to this project. She validated my findings as well. 😉

Through this project, I learned how crimes distribute in Houston and gained a clear understanding of how Bayesian models work. 

For the full analysis, feel free to check it out! [Bayesian Crime Map](./Bayesian_Crime_Map.pdf)
