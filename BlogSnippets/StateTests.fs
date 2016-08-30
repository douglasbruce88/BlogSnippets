namespace BlogSnippets

module StateTests = 

    open NUnit.Framework
    open Swensen.Unquote
    open BlogSnippets.State

    [<Test>]
    let ``State monad``() = 
      let list = [ 1..10 ]
      let actual = stateSum list
      let expected = List.sum list
      actual =! expected
      

