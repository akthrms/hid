import Data.Queue
import Flow ((|>))
import qualified System.Exit as Exit

main :: IO ()
main =
  do
    let q =
          empty
            |> enqueue 0
            |> enqueue 5
            |> enqueue 10
            |> enqueue 15

        q' =
          q
            |> dequeue
            |> dequeue

        q'' =
          q'
            |> enqueue 100

        shouldBeTrue =
          [ front q' == Just 10,
            front q'' == Just 10,
            q'
              |> dequeue
              |> dequeue
              |> isEmpty
          ]

    if and shouldBeTrue
      then pure ()
      else Exit.exitFailure
