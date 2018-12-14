---
title: "How to make game in the browser thanks to ScalaJS"
date: 2018-09-18T15:30:53+02:00
draft: false
description: ScalaIO.2018 Workshop
keywords:
  - Scala
  - ScalaJS
  - ScaoaIO
  - Frontend
---

A few month ago, the [Paris Scala User Group](https://www.meetup.com/fr-FR/Paris-Scala-User-Group-PSUG/) kindly invited me present a [workshop](https://www.meetup.com/fr-FR/Paris-Scala-User-Group-PSUG/events/251045516/) introducing [Scala.js(https://www.scala-js.org/). Even better, i will have the chance to present it in October at [ScalaIO](https://scala.io/talks.html#/#PGM-9348).

I will present how to develop a web application in Scala.js. This talk is for Scala developers having a penchant for web development but rebuffed by JavaScript. It goes from ScalaJS basics to the implementation of a naive virtual DOM written in Scala. It presents:

- how to setup Sbt for cross compilation
- what is the DOM and how to manipulate it
- events and their propagation
- the Model/View/Update architecture (a.k.a the ELM architecture)

The final result can be experimented with at [chrilves.github.io/slimetrail](https://chrilves.github.io/slimetrail). The english material for the ScalaIO workshop are not yet available but the ones for the PSUG workshop, in french are [here](https://github.com/chrilves/slimetrail.scalajs).

# The Application

<iframe width="1024" height="768" src="https://chrilves.github.io/slimetrail"></iframe>