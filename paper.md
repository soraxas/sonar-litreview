---
link-citations: true

codeBlockCaptions: True
figureTitle: |
  Figure
figPrefix:
  - "Fig."
  - "Figs."
eqnPrefix:
  - "Eq."
  - "Eqs."
tblPrefix:
  - "Table"
  - "Tables"
secPrefix:
  - "Sec."
  - "Secs."
lastDelim: " and "
linkReferences: true
nameInLink: true
lofTitle: |
  ## List of Figures
lotTitle: |
  ## List of Tables
tableTemplate: |
  *$$tableTitle$$ $$i$$*$$titleDelim$$ $$t$$
autoSectionLabels: True
title: pandoc-crossref demo document
---

# Introduction



For example, the depth of the ocean can usually be measured by ascoustic echo-sounders on ships, or by satellite altimeters data.
Ascoustic devices are widely adopted for systematic survey of ocean basin.
Oceanographic and naval ships have operated echo sounders almost continuously while at sea, which faciliate millions of miles of ship-track data recorded to
produce maps.
The tracks are not well distributed and tends to clustered in regions that are high in traffic, such as commercial shipping routes.
@fig:echo-sounder-location-australia illustrates an example of the echo-sounder data used for mapping the ocean floor near Australia.

<div id="fig:echo-sounder-location-australia">
  ![](echo-sounder-location-australia.png)

Locations of echo-sounds data used for ocean floor mapping near Australia [@smith1996_ShipTrac]

</div>


For example, [@hahner2019_SemaUnde]
For example, [@johnson-roberson2017_DrivMatr]


# Information

Autonomous Underwater Systems

* [REMUS 100](https://auvac.org/213-2/)
* [REMUS 600](https://www.naval-technology.com/projects/remus-600-autonomous-underwater-vehicle-auv/)
* [L3 Harris Iver3](https://www.l3harris.com/all-capabilities/iver3-standard-system-uuv)
* [Bluefin 9](https://gdmissionsystems.com/products/underwater-vehicles/bluefin-9-autonomous-underwater-vehicle)
* [ISE Explorer AUV](https://ise.bc.ca/product/explorer/)





Multi-Apertere Somar (MAS) uses a dynamically focused hiufltkahi

Active synthetic aperture sonar (SAS) is an enhancement from the standard, narrow beamwidth, sidescan sonar; capable of producing a more faithful, optical-like "image" of the seafloor. The image intensity represent the back scattered acoustic energy from a specific direction.


Synthetic aperture radar (SAR) satellites collect swaths of side-looking echoes at a sufficiently high range resolution and along-track sampling rate to form high resolution imagery. For example,


# Background Information

Sonar (**SO**ound **N**avigation **A**nd **R**anging) and Radar (**RA**dio **D**etection **A**nd **R**anging) are two system that operates under the same principle but with different types of wave.
Both systems are established to estimate the position of some foreign objects using waves [@altes1979_TargPosi].
Sonar uses ultrasound for detection underwater, whereas Radar uses radio waves for above the land or underwater detection.
Since their usage, techniques and terminology are often overlapping with literature, in the following, we shall briefly review techniques that are used in both systems and contrast the two.
Then, we will focus on the current state-of-the-art type of systems within sonar technologies.


Sonar uses sound propagation for navigation, communication, or detecting objects under the water surface. The term "sonar" is shared by two different technologies---_passive sonar_ and _active sonar_. @fig:passive-and-active-sonar illustrates an overview between the two systems.

**Passive Sonar** signal processing performs time delay estimation for naval systems [@carter1981_TimeDela]. Signals received at two or more receiving sensors are used to estimate the position and velocity of some detected acoustic source. While a passive system has the advantage of covertness for military applications, their practicality is highly subjected to background noise as they cannot control the amount of transmitted energy reflected from the source. For example, @howell2003_PassSona and @deseixas2011_PrepPass utilise sensor data retrieved from passive sonar system for performing object detections with neural netowrks.



@howell2003_PassSona [p. 33] says blah.


<div id="fig:passive-and-active-sonar">
  ![](passive-and-active-sonar.png)

  Passive and active sonar system [@kuperman2014_UndeAcou]. **Passive:** submarine (right) passively detects sounds using a towed antenna array. The other submarine will receive machine noise on the left (blue) and surface shipping noise (red). These sounds are distorted by the shallow-water environment and are embedded in ocean surface noise (green). **Active:** ship on the right sends out a pulse (red) and receives an echo (blue), regardless of machine noise, which is in turn distorted by the shallow-water environment. The echo is returned to the ship, which needs to be distinguished from backscattered reverberation (yellow) and ocean noise (green). Various environmental factors (noises) are discussed in @sec:sonar-speed XXXXXXXXXXXXXXXXX
</div>






# Characteristic of Sonarr



## Variability of the Speed of Sound {#sec:sonar-speed}

The ocean's acoustic properties mainly depend on the ocean sound speed structure, which in turn depends on the osceangraphic environment.
When a sound wave travels along a path from some localised source, the combination of the water column and bottom properties leads to a set of sound-propagation profiles [@medwin1999_FundAcou].
Sound speed profile in the ocean water column has be experimentally derived [@mackenzie1981_NineEqua;@munk1998_AbysReci] as

$$1994$${#eq:speed-of-sound-profile}
```
$$ C = 1448.96 + 4.591T − 0.05304T^2 + 0.0002374T^ 3 + 0.0160Z + (1.340 − 0.01025 T)(S − 35) + 1.675 \times 10^{−7} Z^2 − 7.139 \times 10^{−13} T Z^3 $$
```



where $C$ is the speed of sound in $m/s$, $T$ is the temperature in Celsius, $S$ is salinity (see XXXXXX), and Z is depth in meters (which characterises the ambient pressure).
Note that the formulation details are not of interest in our scope of synthetic sonar modelling.
However, [@eq:speed-of-sound-profile] illustrates the nonlinear relationship between the water temperature and the depth of the acoustic device.
@fig:speed-profile illustrates a visualisation of the same plotted data across ocean depth.
For typical oceanic conditions, $C$ is usually between 1450 m/s and 1550 m/s, which reflects the sensitivity of $C$ to changes of temperature, depth, and salinity.
It is indicative that near-surface sonar devices will tend to experience higher variance inaccuracy due to the polar region near the ocean surface; the polar relationship decays as the depth increases.
In a warmer season, or warmer part of the day, the increased temperature causes sound speed increases toward the sea surface (hence, the influence of $C$ is dominated by the temperature of the water); whereas the pressure term in @eq:speed-of-sound-profile is more significant in deeper water.
These are potential factors to be considered in simulating sonar data for ML modelling, where the environmental factors might be essential conditional features for our ML model to learn.






<div id="fig:speed-profile">
  ![](sound-speed-profile.png)

  Generic sound-speed profiles, which varies directly with temperature and hydrostatic pressure [@kuperman2014_UndeAcou].
  Near-surface mixing leads to isovelocity (the gap in between the curves, which denotes the polar region)
</div>





# Environmental Factors for Simulation {#sec:environmental-factors}


asd

# Background

Autonomous Underwater Vehicles had been increasingly being used by[@chappleAutoDete]

@article{chappleAutoDete,
title = {Automated {{Detection}} and {{Classification}} in {{High-resolution Sonar Imagery}} for {{Autonomous Underwater Vehicle Operations}}},
author = {Chapple, Philip},
pages = {37},
abstract = {Autonomous Underwater Vehicles (AUVs) are increasingly being used by military forces to acquire high-resolution sonar imagery, in order to detect mines and other objects of interest on the seabed. Automatic detection and classification teclmiques are being developed for several reasons: to provide reliable and consistent detection of objects on the seabed; to free human analysts from time-consuming and tedious detection tasks; and to enable autonomous in-field decision-making based on observations of mines and other objects. This document reviews progress in the development of automated detection and classification teclmiques for side-looking sonars mounted on AUVs. Whilst the teclmiques have not yet reached maturity, considerable progress has been made in both unsupervised and supervised (trained) algoritluns for feature detection and classification. In some cases, the perfonnance and reliability of automated detection systems exceed those of human operators.},
langid = {english},
keywords = {⛔ No DOI found}
}

Sampling-based motion planning is one of the fundamental methods by which robots navigate and integrate with the real world [@elbanhawi2014_SampRobo].
Motion planning involves planning the trajectories of the actuated part of the robot, under various constraints, while avoiding collisions with surrounding obstacles.
Sampling-based motion planners (SBPs) are robust methods that avoid explicitly constructing the often intractable high-dimensional configuration space (C-Space).
Instead, SBPs randomly sample the C-Space for valid connections and iteratively build a roadmap of connectivity.
Most SBPs are guaranteed to find a solution if one exists [@kavraki1996_AnalProb], and such a planner is said to be _probabilistic complete_.
A further development for SBPs is _asymptotic optimality_[@elbanhawi2014_SampRobo]: a guarantee that the method will converge, in the limit, to the optimal solution.

SBPs are applicable to a wide range of applications.
Example include planning with arbitrary cost maps [@iehlCostmapPlanningHigh2012], cooperative multi-agent planning [@jinmingwuCooperativePathfindingBased2019], and planning in dynamic environments [@yershova2005_DynaRRTs].
On the one hand, researchers have focused on the algorithmic side of improving the graph or tree building [@lai2018_BalaGlob;@klemmRRTConnectFaster2015;@zhongTripleRrtsRobotPath2012;@elbanhawi2014_SampRobo;@lai2021lazyExperienceGraph;@lai2021rapidlyexploring].
On the other hand, the advancement of neural networks allows an abundance of learning approaches to be applied in SBPs [@strubAdaptivelyInformedTrees2020;@bagnell2014_ReinLear] and on improving the sampling distribution [@alcin2016_ExtrLear;@lai2020_BayeLoca;@lai2021plannerFlows;@laiLearningPlanOptimally2020;@lai2021diffSamp].

# Statement of need

The focus of motion planning research has been mainly on (i) the algorithmic aspect of the planner using different routines to build a connected graph and (ii) improving the sampling efficiency (with methods such as heuristic or learned distribution). Traditionally, robotic research focuses on algorithmic development, which has inspired several motion planning libraries written in C++, such as Move3D [@simeon2001move3d] and OMPL [@sucan2012open]. In particular, OMPL has been one of the most well-known motion planning libraries due to its versatility, and it has been a core part of the planning algorithm used in the MoveIt framework [@chitta2012moveit]. However, swapping the sampler within each planner is very restrictive, as planners are typically hard-coded to use a specific sampler. In addition, it is cumbersome to integrate any learning-based approach into a framework as there is only a limited number of choices of deep-learning libraries in C++.

Python has been a popular language to use in Machine Learning due to its rapid scripting nature. For example, PyTorch [@paszke2019pytorch] and Tensorflow [@abadi2016tensorflow] are two popular choices for neural network frameworks in Python. A large number of learning approaches are available as Python packages. It shall be noted that the aforementioned OMPL has Python bindings available; however, OMPL uses an outdated Py++ code generator, and every modification to the source code will require hours to updates bindings plus recompilation.
Some Python repositories are available that are dedicated to robotics motion planning [@sakai2018pythonrobotics]; however, most only showcase various planning algorithms, without an integrated environment and simulators.

![Implementation details on the class hierarchy structure of `sbp-env`.\label{fig:class-diagram}](class_diagram.png)

# Overview

We introduce `sbp-env`, a _sampling-based motion planners' testing environment_, as a complete feature framework to allow rapid testing of different sampling-based algorithms for motion planning.
`sbp-env` focuses on the flexibility of tinkering with different aspects of the framework, and it divides the main planning components into two main categories: (i) samplers and (ii) planners.
The division of the two components allows users to decouple them and focus only on the component that serves as the main focus of the research.
`sbp-env` has implemented the entire robot planning framework with multiple degrees-of-freedom, which allows benchmarking motion planning algorithms with the same planner under different backend simulators.
Separating the two components allows users to quickly swap out different components in order to test novel ideas.

Building the framework enables researchers to rapidly implement their novel ideas and validate their hypotheses.
In particular, users can define the environment using something as simple as an _image_, or as complicated as an _xml file_.
All samplers and planners can be added as a plugin system, and `sbp-env` will auto-discover newly implemented planners or samplers that have been added to the dedicated folders.

Figure \ref{fig:class-diagram} illustrates the hierarical structure of our package.
Our implementation of `sbp-env` define abstract interfaces for **sampler** and **planners**, from which all corresponding concrete classes must inherit.
In addition, there are classes that represent the full-body simulations of the environments and the corresponding visualisation methods.
Note that all visualisation can be turned off on-demand, which is beneficial when users benchmark their algorithms.
The docunmentation of `sbp-env` is available at [https://cs.tinyiu.com/sbp-env](https://cs.tinyiu.com/sbp-env).

\listoffigures
[]: hack to split raw blocks
\listoftables
[]: hack to split raw blocks
\listoflistings

This is a demo file for pandoc-crossref. With this filter, you can cross-reference figures (see [@fig:figure1;@fig:figure2;@fig:figure3]), display equations (see @eq:eqn1), tables (see [@tbl:table1]) and sections ([@sec:sec1; @sec:sec2; @sec:caption-attr; @sec:table-capts; @sec:wrapping-div])

For immediate example, see @fig:figure0.

For immediate example, see @fig:figfigfig.


For immediate example, see @fig:asda.



$$\alpha$$

<div id="fig:asda">
![](img2.jpg)

This is my caption
</div>

<div id="fig:figfigfig">
![](img2.jpg)

This is my caption
</div>

![A figure](img1.jpg){#fig:figure0}

There is also support for code blocks, for example, [@lst:captionAttr; @lst:tableCaption; @lst:wrappingDiv]

It's possible to capitalize reference prefixes, like this: [@Fig:figure1].

In case of multiple references, capitalization is determined by first reference. [@Fig:figure1; @fig:figure2] is capitalized, while [@fig:figure2; @Fig:figure1] is not.

It is also possible to mix different references, like [@fig:figure1; @tbl:table1; @lst:captionAttr; @lst:tableCaption; @fig:figure2; @fig:figure3], which will be grouped in order they are specified. You can even intermix this with regular citations, although it's not recommended: [@fig:figure1; @tbl:table1; @unprocessedCitation]

You can also have custom chapter reference labels, like @sec:custlabs

Subfigures are supported, see [@fig:subfigures; @fig:subfigureB]

# Chapter 1. Figures {#sec:sec1}

![First figure](img1.jpg){#fig:figure1}

![Second figure](img2.jpg){#fig:figure2}

![Third figure](img3.jpg){#fig:figure3}

![Unlabelled image](img1.jpg)

<div id="fig:subfigures">
![Subfigure a](img1.jpg)

![Subfigure b](img1.jpg){#fig:subfigureB}

Subfigures caption
</div>

# Chapter 2. Equations {#sec:sec2}

Display equations are labelled and numbered

$$ P_i(x) = \sum_i a_i x^i $$ {#eq:eqn1}

Since 0.1.6.0 those can also appear in the middle of paragraph
$$a x^2 + b x^2 + c = 0$${#eq:quadr} like this.

# Chapter 3. Tables

| First Header | Second Header |
|:-------------|:--------------|
| Content Cell | Content Cell  |
| Content Cell | Content Cell  |

: Table example {#tbl:table1}

Table without caption:

| First Header | Second Header |
|:-------------|:--------------|
| Content Cell | Content Cell  |
| Content Cell | Content Cell  |

# Chapter 4. Code blocks

There are a couple options for code block labels. Those work only if code block id starts with `lst:`, e.g. `{#lst:label}`

## `caption` attribute {#sec:caption-attr}

`caption` attribute will be treated as code block caption. If code block has both id and `caption` attributes, it will be treated as numbered code block.

```{#lst:captionAttr .haskell caption="Listing caption"}
main :: IO ()
main = putStrLn "Hello World!"
```

\pagebreak

## Table-style captions  {#sec:table-capts}

Enabled with `codeBlockCaptions` metadata option. If code block is immediately
adjacent to paragraph, starting with `Listing:` or `:`, said paragraph will be
treated as code block caption.

Listing: Listing caption

```{#lst:tableCaption .haskell}
main :: IO ()
main = putStrLn "Hello World!"
```

## Wrapping div

Wrapping code block without label in a div with id `lst:...` and class, starting with `listing`, and adding paragraph before code block, but inside div, will treat said paragraph as code block caption.

<div id="lst:wrappingDiv" class="listing">
Listing caption
```{.haskell}
main :: IO ()
main = putStrLn "Hello World!"
```
</div>

# Unnumbered chapter. {-}

<!-- This chapter doesn't change chapter prefix of referenced elements, instead keeping number of previous chapter, e.g.
$$ S(x) = \int_{x_1}^{x_2} a x+b \  \mathrm{d}x $$ {#eq:eqn2} -->

# Chapter 5. Reference lists

It's also possible to show lists of figures and tables, like this:

\listoffigures

\listoftables

\listoflistings

# Appendix A. Custom labels {label=AppA}

## This section will have custom label {#sec:custlabs label=CustLab}

# References
