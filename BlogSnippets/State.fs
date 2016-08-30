namespace BlogSnippets

module State = 
    // How to perform an action
    let startVal = 1
    let endVal = 10
    
    let forLoop act = 
        for i = startVal to endVal do
            act (i)
    
    let whileLoop act = 
        let mutable i = startVal
        while i <= endVal do
            act (i)
            i <- i + 1
    
    let recLoop act = 
        let rec aux i = 
            match i > endVal with
            | true -> ()
            | _ -> 
                act (i)
                aux (i + 1)
        aux startVal
    
    let action i = printf "%d " i
    
    forLoop action
    printf "%s" System.Environment.NewLine
    whileLoop action
    printf "%s" System.Environment.NewLine
    recLoop action
    printf "%s" System.Environment.NewLine
    
    // Summing a list
    let forSum list = 
        let mutable sum = 0
        for elt in list do
            sum <- sum + elt
        sum
    
    let whileSum list = 
        let mutable sum = 0
        let mutable i = 0
        while i < List.length list do
            sum <- sum + list.[i]
            i <- i + 1
        sum
    
    let recSum list = 
        let rec aux remainderOfList sumSoFar = 
            match remainderOfList with
            | [] -> sumSoFar
            | head :: tail -> aux tail (sumSoFar + head)
        aux list 0
    
    let list = [ startVal..endVal ]
    
    printf "%d %s" (forSum list) System.Environment.NewLine
    printf "%d %s" (whileSum list) System.Environment.NewLine
    printf "%d %s" (recSum list) System.Environment.NewLine
    
    // Using an explicit state type
    let (>>=) x f = 
        (fun s0 -> 
        let a, s = x s0
        f a s)
    
    let returnS a = (fun s -> a, s)
    
    type StateBuilder() = 
        member m.Bind(x, f) = x >>= f
        member m.Return a = returnS a
        member m.ReturnFrom(x) = x
    
    let state = new StateBuilder()
    let getState = (fun s -> s, s)
    let setState s = (fun _ -> (), s)
    let Execute m s = m s |> snd
    let test s = s
    
    let stateSum list = 
        let rec aux t = 
            state { 
                match t with
                | head :: tail -> 
                    let! s = getState
                    do! setState (s + head)
                    return! aux tail
                | [] -> let! s = getState
                        return ([], s)
            }
        Execute (aux list) 0
    
    printf "%d %s" (stateSum list) System.Environment.NewLine
