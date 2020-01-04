open Animals
open System
open System.IO
open System.Text

let mutable mice : (mouse list) = [
    mouse(p,true,(0,0)) //parameters: p is the counter, true refers to it being the original mouse, 
    // and (0,0) is the starting position.
    ]

let mutable owls : (owl list) = [
    owl() //owl needs no parameters.
    ]

/// <summary> Checks if all mice are dead or not </summary>
/// <returns> Returns a bool, true if all mice are dead.</returns>
let checkIfAllDead () =
    let mutable aliveList = []
    for a in mice do
        aliveList <- aliveList @ a.getIsAlive
    let nmleft = aliveList |> List.forall (fun i -> i = false) 
    nmleft

/// <summary> Checks if no mice are no mouse objects left </summary>
/// <returns> Returns a bool, true if all mice are dead.</returns>
let noMiceLeft()=
    if mice.Length = 0 then
        true
    else
        false  

/// <summary> Starts a new simulation and writes results to a .txt file </summary>
let write () =
    let outFileName = "simulation.txt" //name of the file that is written on
    use outFile = new StreamWriter(outFileName, false) //creates a stream of writing

    /// <summary> Moves every owl, and does nothing if no mice are left </summary>
    let moveOwls () =
        if noMiceLeft() then //checks if there are any mice left        
            outFile.WriteLine(sprintf  "Ending simulation, all mice gone." )
        else  
        for i in owls do //moves every owl
            i.moveAnimal  

    /// <summary> Calling this is a whole tick for all owls </summary>
    let calling () =
        moveOwls ()  //starts with moving the owls
        outFile.WriteLine(sprintf  "The owls have finished their turn, and are currently at: %A" Environment.getOwlList)
        for i in owls do //checks if the mices are gone in any of the owls turns
            if checkIfAllDead() then        
                outFile.WriteLine(sprintf  "Ending simulation, all mice gone." )
            else 
                if (i.Position = i.findNMPos) then // if the owl position is the same as the target mouse.
                    outFile.WriteLine(sprintf  "Owl number %A has eaten a mouse at field: %A" i.getOwlID i.findNMPos  )  
                    let owlposi = mice |> List.collect (fun m -> //finds all mice on the owl position
                        if m.Position = i.Position then 
                            m.killMouse() // the mouses isAlive status is set to false
                            m.MoveAwayFromEnvironment() // the mouses pos is set in a position outside the environment
                            [(m).getIsAlive]
                        else
                            [(m).getIsAlive]               
                            )            
                    i.mouseEaten i.Position //removes the mouse that was eaten
                    outFile.WriteLine(sprintf  "Remaining mice: %A\n" i.mousList )


    /// <summary> Calling this is a whole tick for all mice </summary>
    /// <param name = "id"> int , representing the id of the mouse</param>
    let lowerPAndMove (id) = 
        let mutable hasSpaceToMove = true
        if noMiceLeft() then //checks if the mice are gone        
            outFile.WriteLine(sprintf  "Ending simulation, all mice gone." )
        else   
            if mice.[id].getP = 1 then // if the mouse is about to multiply
                if (mice.[id].ifSpacesAreEmpty mice.[id].Position) then //checks if there is space to multiply
                    outFile.WriteLine(sprintf  "Mouse number: %A has multiplied, and is currently at: %A" mice.[id].getMouseID mice.[id].Position)
                    mice <- mice @ [mouse(mice.[id].MoriginalP, false,mice.[id].Position)]//add new mouse to the mice list, false means it is not an original mouse
                    mice.[id].lowerCounter() //lower the p, which puts it back in the original p
                else
                    hasSpaceToMove <- false  //marks that that the mouse has no available spaces
                    outFile.WriteLine(sprintf  "Mouse number: %A did not move or multiply, since it has no space to multiply."  mice.[id].getMouseID    ) 
            else
                if hasSpaceToMove then //if the mouse has space to move and is not about to multiply
                    mice.[id].lowerCounter() //lower the p 
                    mice.[id].moveAnimal //Moves the mouse

    /// <summary> Call mice to move that are alive </summary>
    let moveMice () = 
        if checkIfAllDead() then        
            outFile.WriteLine(sprintf  "Ending simulation, all mice gone." )
        else   
            for i in mice do //for every mouse
                if i.getIsAlive.Head then //checks if it is alive         
                    lowerPAndMove(i.getMouseID) //calls to lower the p and move if it is alive
            outFile.WriteLine(sprintf  "The mice have finished their turn, and are currently at: %A" Environment.getMouseList   )         
               

    /// <summary> Controls the ticks of the simulation </summary>
    let callOandM () =
        outFile.WriteLine(sprintf "The starting position for the mice are: %A" Environment.getMouseList )
        outFile.WriteLine(sprintf  "The starting position for the owls are: %A\n" Environment.getOwlList )
        for s in 1..T do //for every tick         
            if checkIfAllDead() then //ends the simulation if there are no mice left       
                outFile.WriteLine(sprintf  "Ending simulation, no mice left." )
            else         
                outFile.WriteLine(sprintf  "\nTick: %A --------------------------------------------------\n" s   )   
                moveMice () //first calls mice to take a turn
                if checkIfAllDead() then        
                    outFile.WriteLine(sprintf  "Ending simulation, all mice gone.")
                else  //then calls all owls to make a turn                       
                    calling ()

    /// <summary> Adds mice and owls to their classes </summary>
    /// <param name = "M"> int , representing the amount of mice specified</param>
    /// <param name = "O"> int , representing the amount of owls specified</param>
    /// <param name = "p"> int , representing the p counter</param>
    let addOwlsAndMice M O p =
        outFile.WriteLine(sprintf "\nThe simulation starts with %A mice and %A owls." M O)
        outFile.WriteLine(sprintf "The environment consist of %A × %A fields organized as a checkerboard." n n)
        outFile.WriteLine(sprintf "The simulation runs on %A ticks, and after each %A tick, all surviving mice will multiply." T p)
        for i in 2..(M) do // adds mice to the list, which will always start with atleast one mouse
            mice <- mice @ [mouse(p, true,(0,0))]
        for i in 2..(O) do // adds owl to the list, which will always start with atleast one owl
            owls <- owls @ [owl()]    
        callOandM () // starts the simulation   

    addOwlsAndMice (M) (O) p //first call of the simulation, adding objects to classes and starting the simulation

    for i in mice do // after the simulation is done, this prints position of all remaining mice
        if i.getIsAlive.Head then
            outFile.WriteLine(sprintf "Surviving mouses positions: %A" i.Position)  

write() //calls the simulation to start          

// printfn "\nWhite-box testing."
// printfn " Unit: vector "
// printfn "   Branch : 0a %b" ((vector (0,0) (4,8)) = (4,8))
// printfn "   Branch : 0b %b" ((vector (4,8) (0,0)) = (-4,-8))
// printfn "   Branch : 0c %b" ((vector (0,0) (n,n)) = (n,n))
// printfn "   Branch : 0d %b" ((vector (5,5) (5,5)) = (0,0))
// printfn "   Branch : 0e %b" ((vector (0,0) (9999,9999)) = (9999,9999))

// printfn " Unit: findNearestMouse "
// printfn "   Branch : 0a %A" (mice.findNearestMouse (Environment.getMouseList Environment.getOwlList))
// printfn "   Branch : 0b %b" ((vector (4,8) (0,0)) = (-4,-8))
// printfn "   Branch : 0c %b" ((vector (0,0) (n,n)) = (n,n))
// printfn "   Branch : 0d %b" ((vector (5,5) (5,5)) = (0,0))
// printfn "   Branch : 0e %b" ((vector (0,0) (9999,9999)) = (9999,9999))