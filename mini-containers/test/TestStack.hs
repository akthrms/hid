import Data.Stack
import Flow ((|>))
import qualified System.Exit as Exit

main :: IO ()
main =
  do
    let s =
          empty
            |> push 0
            |> push 5
            |> push 10
            |> push 15

        s' =
          s
            |> pop
            |> pop

        s'' =
          s'
            |> push 100

        shouldBeTrue =
          [ top s' == Just 5,
            top s'' == Just 100,
            s'
              |> pop
              |> pop
              |> isEmpty
          ]

    if and shouldBeTrue
      then pure ()
      else Exit.exitFailure
