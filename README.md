Swing-DSL
========================

<i> Scala-swing forms and layouts building DSL </i>

<a href="https://github.com/fehu/dsl">DSL Root project<a/>

Usage examples:

     lazy val layoutChooserBuilder = controlForSeq[String](graphLayoutNames, static = true)
       .dropDownList(name => {
         currentGraphLayout = layoutByName(name)
         updateGraphVisualization()
     })

    val nodeInfo = monitorFor(selectedVertex).text
      .extractFuture(extractAsync _ andThen (_.map(Xhtml.toXhtml)))

    def extractAsync: Option[Id] => Future[NodeSeq]



Using <a href="https://github.com/fehu/swing-dsl/blob/1e6237a57d8a6dc0a08cae603d929215d0040db9/src/main/scala/feh/dsl/swing/SwingAppFrame.scala">
    Frame9PositionsLayoutBuilder/GridBagLayoutBuilder </a>:

    lazy val controlPanelBuilder = panel.gridBag(
      place(scrollable()(nodeInfo, "node-info")
          .fillBoth
          .yWeight(.9)
          .xWeight(1)
          .insets(10)(top = 0, right = 5)
        ) in theCenter,
      place(leftTopPanelBuilder
          .fillBoth
          .xWeight(1)
          .insets(10)(right = 5)
        ) to theNorth of "node-info",
      place(leftBottomPanelBuilder
          .fillBoth
          .xWeight(1)
          .insets(10)(top = 0, right = 5)
        ) to theSouth of "node-info"
    )
