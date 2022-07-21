@main

object Main{

  val params = InputOutput.parseArgs()
  var world: World = World.apply(params)
  world.run()

}






// write the InputOutput class to define the outputs that are to be used by OM
// check what OM needs, if scala variables or files...
