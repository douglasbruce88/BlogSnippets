namespace BlogSnippets

module StateTests = 

    open NUnit.Framework
    open Swensen.Unquote
    open BlogSnippets.State

    [<Test>]
    let ``Summing a list``() = 
       let list = [ 1..10 ]
       let a = forSum list
       let b = whileSum list
       let c = recSum list
       a =! b
       b =! c
     
    [<Test>]
    let ``Local state``() = 
      let list = [ 1..10 ]
      let actual = stateSum list
      let expected = List.sum list
      actual =! expected    
      
    [<Test>]
    let ``Generic aggregation``() = 
      let list = [ 1..10 ]
      let actual = fold (+) 0 list
      let expected = List.sum list
      actual =! expected    

    [<Test>]
    let ``Global mutable state``() = 
      for _ in 1 .. 5 do canOnlyRunFiveTimes (printf "Run %d \r\n") (printf "Not run %d \r\n")
      globalCounter =! 5
      canOnlyRunFiveTimes (printf "Run %d \r\n") (printf "Not run %d \r\n")
      globalCounter =! -1

    [<Test>]
    let ``Global monadic state``() =
      let m = canOnlyRunFiveTimesWithStateMonad (printf "Run %d \r\n") (printf "Not run %d \r\n") 
      let composedFiveTimes =  m >=> m >=> m >=> m >=> m
      let composedSixTimes =  composedFiveTimes >=> m
      Execute composedFiveTimes 0 =! 5
      Execute composedSixTimes 0 =! -1
      

