module Animals

open System
open System.IO

printfn "The environment consist of n × n fields organized as a checkerboard"
printfn "Enter a number n: "
let n = Console.ReadLine() |> int
printfn "The simulation updates in ticks, and after each tick, all animals perform an action"
printfn "Enter a number that represents the number of ticks: "
let T = Console.ReadLine() |> int
printfn "There must initially be O owls and M mice"
printfn "Enter a number that represents the number of owls: "
let O = Console.ReadLine() |> int
printfn "Enter a number that represents the number of mice: "
let M = Console.ReadLine() |> int
printfn "A mouse must have a counter, such that after p ticks,the mouse will not move but multiply."
printfn "Enter a number that represents the number p: "
let p = Console.ReadLine() |> int

let rand = new System.Random()

/// <summary> Finds the vector of 2 given points</summary>
/// <param name = "(x0y0)"> A tuple of floats , representing the first coordinate</param>
/// <param name = "(x1y1)"> A tuple of floats , representing the second coordinate</param>
/// <returns> Returns a single tuple of floats. The vector of the 2 points.
/// The first float returned is the distance on the x axis and the second is the y axis  </returns>
let vector ((x0y0) : int * int) ((x1y1) : int * int) =  //creates a vector from 2 points
    ((fst(x1y1) - fst(x0y0)),((snd(x1y1)) - snd(x0y0))) 

/// <summary> Selects either 1 or -1 randomly </summary>
/// <returns> Returns only 1 or -1 chosen at random </returns>
let findOneOrMOne () = //finds a random number, either 1 or - 1
    let a = (rand).Next(1, 3) //picks either 1 or 2
    if a = 1 then 1
    else -1

type Environment (animal : int , originalMouse,pos) =
    let mutable whichAnimal = animal //mouses are 0 and owls are 1
    let startingPosition = 
        if whichAnimal = 1 then //if animal is an owl
            ((rand).Next(0, n),(rand).Next(0, n)) //random position from (0,0) - (n,n)
        else            
            if originalMouse then //if animal is a mouse, and an original mouse
                ((rand).Next(0, n),(rand).Next(0, n)) //random position from (0,0) - (n,n)
            else
                pos //sets the position of the mouse it originated from
    let mutable currentPosition = startingPosition

    /// <summary> Makes sure the object does not go outside the boundaries of the environment </summary>
    /// <returns> Returns a new position inside the boundaries, if the object tried to go out of bounds </returns>
    let checkBoundaries () = //redirects the animal if it tries to go outside map
        if fst(currentPosition) < 0 then //checks if mouse goes left of map
             currentPosition <- (fst(currentPosition) + 1,snd(currentPosition))       
        if fst(currentPosition) > n then //checks if mouse goes right of map
             currentPosition <- (fst(currentPosition) - 1,snd(currentPosition))  
        if snd(currentPosition) < 0 then //checks if mouse goes below map
             currentPosition <- (fst(currentPosition),snd(currentPosition) + 1)
        if snd(currentPosition) > n then //checks if mouse goes above map
             currentPosition <- (fst(currentPosition) ,snd(currentPosition) - 1) 

    //mouse position list
    static let mutable allmousepos = []
    //owl position list
    static let mutable allowlpos = []

    /// <summary> Finds a vector for an owl to the nearest mouse </summary>
    /// <param name = "m"> int * int list , each element representing a mouse position</param>
    /// <param name = "o"> int * int , representing the position of the owl in question</param>
    /// <returns> Returns a vector (int * int) The direction the owl has to go to. </returns>
    let findNearestMouse (m : (int*int)list) (o : (int*int)) = 
        m   |> List.map (fun x -> (vector o x)) //finds the vector for every mouse and the owls position
            |> List.map (fun x -> (abs(fst x) + abs(snd x)),x) //finds how many moves are needed to reach a mouse      
            |> List.min //finds the smallest number
            |> snd //returns the vector needed

    /// <summary> Checks if a space is empty or not for mice</summary>
    /// <param name = "spaceInQuestion"> int * int , representing the position in question</param>
    /// <returns> Returns a bool, false if it is occupied, else, true.</returns>
    let checkIfspaceIsEmptyForMice (spaceInQuestion) =
        if (List.contains spaceInQuestion (allmousepos @ allowlpos)) then //If either a mice or an owl is at spaceInQuestion
            false //returns false if it is occupied
        else
            true //returns true if it is available

    /// <summary> Checks if a space is empty or not for owls </summary>
    /// <param name = "spaceInQuestion"> int * int , representing the position in question</param>
    /// <returns> Returns a bool, false if it is occupied, else, true.</returns>
    let checkIfspaceIsEmptyForOwls (spaceInQuestion) =
        if (List.contains spaceInQuestion (allowlpos)) then //same as the function above, but the owls can enter the mices field
            false
        else
            true        

    /// <summary> finds the nearest available position and moves the object, does not move if there are no available spaces </summary>
    /// <returns> Returns a new currentposition, either moved or not </returns>
    let findNextPos () = //next pos for mice
        let allPossibleMoves = //list of all possible moves         
            [fst(currentPosition) + 1,snd(currentPosition) + 0; 
            fst(currentPosition) + 0,snd(currentPosition) - 1;      
            fst(currentPosition) - 1,snd(currentPosition) + 0; 
            fst(currentPosition) + 0,snd(currentPosition) + 1; 
            ] |> List.filter (fun i -> checkIfspaceIsEmptyForMice i) //takes out all occupied spaces 
        let listL = allPossibleMoves.Length //number of possibles moves
        if allPossibleMoves.IsEmpty then //if there is no available spaces
            currentPosition //does not move if there are no available spaces
        else
            let randomIndex = (rand).Next(0, listL) // Picks a random number that is <= 0 <= possible moves
            checkBoundaries()
            allPossibleMoves.[randomIndex] //returns the new position, randomly selected from available fields            
            
   
    
    do if whichAnimal = 1 then //Places all owls at a random neighbouring position, to make sure they dont start at the same field
        currentPosition <- (findNextPos ()) //places the owl on a square that is not occupied
        checkBoundaries() //makes sure it is not outside of the environment
        allowlpos <- allowlpos @ [currentPosition] //adds the position to a list of all owls
    do if whichAnimal = 0 && originalMouse then
        currentPosition <- (findNextPos ()) //places the owl on a square that is not occupied
        checkBoundaries()
        allmousepos <- allmousepos @ [currentPosition]
    do if whichAnimal = 0 && not originalMouse then       
        currentPosition <- pos //placed at the same place as the mouse that multiplied
        currentPosition <- (findNextPos ())
        checkBoundaries() //finds the next available spot 
        do allmousepos <- allmousepos @ [currentPosition] //adds new mouse to the list of mouse positions
    
    member this.getDist = findNearestMouse 
    member this.findNM = findNearestMouse allmousepos currentPosition
    member this.findNMPos = (fst(findNearestMouse allmousepos currentPosition) + fst(currentPosition),snd(findNearestMouse allmousepos currentPosition)+snd(currentPosition))
    //get lists
    static member getOwlList with get () = allowlpos
    static member getMouseList with get () = allmousepos

    /// <summary> Removes a mouse position that has been eaten </summary>
    /// <param name = "owlPos"> int * int , representing the position of a mouse that has been eaten</param>
    member this.mouseEaten (owlPos) = //Removes an eaten mouse from the mouse position list
        allmousepos <- allmousepos |> List.filter (fun s -> (not (s = (owlPos))))

    /// <summary> Checks if a space is empty or not for owls </summary>
    /// <param name = "mousePos"> int * int , representing the position in question</param>
    /// <returns> Returns a bool, false if it is occupied, else, true.</returns>
    member this.ifSpacesAreEmpty (mousePos) =      
        let allPossibleMoves = //list of all possible moves         
            [fst(currentPosition) + 1,snd(currentPosition) + 0; 
            fst(currentPosition) + 0,snd(currentPosition) - 1;      
            fst(currentPosition) - 1,snd(currentPosition) + 0; 
            fst(currentPosition) + 0,snd(currentPosition) + 1; 
            ] |> List.filter (fun i -> checkIfspaceIsEmptyForMice i) //takes out all occupied spaces 
        let listL = allPossibleMoves.Length //number of possibles moves
        if allPossibleMoves.IsEmpty then 
            false
        else
            true

    member this.MoveAwayFromEnvironment () = //places mice at maximum distance so owls will never target them
        currentPosition <- (-n - 1,-n - 1)    
    member this.mousList = allmousepos
    member this.Position = currentPosition
    /// <summary> Moves the animals, both mice and owls. </summary>
    member this.moveAnimal = 
        if whichAnimal = 0 then   //if it is a mouse
            currentPosition <- (findNextPos ()) //finds the next move
            checkBoundaries() //checks if went out of bounds                                
            allmousepos <- allmousepos.Tail @ [currentPosition] //adds the new position to the mouse position list and removes the old one

        else //if it is an owl
            let mutable allNeighbouringF = //list of all possible moves         
                    [fst(currentPosition) + 1,snd(currentPosition) + 0; 
                    fst(currentPosition) + 0,snd(currentPosition) - 1;     
                    fst(currentPosition) - 1,snd(currentPosition) + 0; 
                    fst(currentPosition) + 0,snd(currentPosition) + 1; 
                    ]     

            let l = allNeighbouringF |> List.map (fun i -> ([(findNearestMouse allmousepos i,i)])) //finds the nearest mouse vector from every possible move
                     |> List.map ((fun i -> abs(fst(fst(i.Head))) + abs(snd(fst(i.Head))), i ) ) // finds the absolute value of the length of each vector
                     |> List.sortBy (fun (i,po) -> i) //sorts the list from closest to furthest
                     |> List.map (fun i -> (snd(snd(i).Head)) ) //extract the possible positions, but now in order from closest to farthest  
                     |> List.find (fun i -> checkIfspaceIsEmptyForOwls i) //finds the closest position that is also free  
            
            currentPosition <- l //moves to the reulting position            
            checkBoundaries() //checks if it went out of bounds                          
            allowlpos <- allowlpos.Tail @ [currentPosition] //adds the new position and removes the old one

type mouse (p,originalMouse,pos) =
    inherit Environment(0, originalMouse,pos)
    let originalP = p
    let mutable isAlive = [true]
    let mutable listOfIsAlive = []
    let mutable Pcounter = p
    static let mutable generateMouseID = 0
    let mouseID = generateMouseID
    do generateMouseID <- generateMouseID + 1
    do listOfIsAlive <- listOfIsAlive @ isAlive
    /// <summary> Lowers the p counter, or refreshed the counter if it is about to multiply </summary>
    member this.lowerCounter () =
        Pcounter <- Pcounter - 1    
        if Pcounter = 0 then
            Pcounter <- originalP  
    /// <summary> Kills the mouse by changin isAlive to false </summary>
    member this.killMouse ()=
        isAlive <- [false]            
    member this.getIsAlive = isAlive
    member this.getMouseID with get () = mouseID
    member this.getP = Pcounter
    member this.MoriginalP = p
    member this.getAliveList = listOfIsAlive

type owl () =
    inherit Environment(1,false,(0,0))
    static let mutable generateOwlID = 0
    let owlID = generateOwlID
    do generateOwlID <- generateOwlID + 1
    member this.getOwlID with get () = owlID


