@main

val params = InputOutput.parseArgs()

val sim = Simulation(0.0, params)
while(sim.hasNext) sim.update

// write the InputOutput class to define the outputs that are to be used by OM
// check what OM needs, if scala variables or files...
