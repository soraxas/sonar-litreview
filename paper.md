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

For example, [@hahner2019_SemaUnde]
For example, [@johnson-roberson2017_DrivMatr]


# Information

Autonomous Underwater Systems

* [REMUS 100](https://auvac.org/213-2/)
* [REMUS 600](https://www.naval-technology.com/projects/remus-600-autonomous-underwater-vehicle-auv/)
* [L3 Harris Iver3](https://www.l3harris.com/all-capabilities/iver3-standard-system-uuv)
* [Bluefin 9](https://gdmissionsystems.com/products/underwater-vehicles/bluefin-9-autonomous-underwater-vehicle)
* [ISE Explorer AUV](https://ise.bc.ca/product/explorer/)

# Background

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
