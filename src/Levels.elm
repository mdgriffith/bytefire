module Levels exposing (..)

import Model exposing (Level, Path(..), Map(..), LineSegment(..), ItemType(..), selectable, AllowedInstructions(..))


levelOne : Level
levelOne =
    { allowed = AllowMove
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 6, y = 5 }
            , Seg { x = 6, y = 5 } { x = 7, y = 5 }
            , Seg { x = 7, y = 5 } { x = 8, y = 5 }
            ]
    , stack = []
    , items =
        [ { x = 8
          , y = 5
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        selectable []
            []
            { instructions =
                [ Nothing, Nothing, Nothing ]
            }
    }


levelTwo : Level
levelTwo =
    { allowed = AllowMoveFn
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 6, y = 5 }
            , Seg { x = 6, y = 5 } { x = 7, y = 5 }
            , Seg { x = 7, y = 5 } { x = 8, y = 5 }
            , Seg { x = 8, y = 5 } { x = 9, y = 5 }
            , Seg { x = 9, y = 5 } { x = 10, y = 5 }
            , Seg { x = 10, y = 5 } { x = 11, y = 5 }
            ]
    , stack = []
    , items =
        [ { x = 11
          , y = 5
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        selectable []
            []
            { instructions =
                [ Nothing, Nothing, Nothing ]
            }
    }


greenTest : Level
greenTest =
    { allowed = AllowAll
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 5, y = 6 }
            ]
    , stack = []
    , items =
        [ { x = 5
          , y = 5
          , kind = Square
          }
        , { x = 5
          , y = 6
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        selectable []
            []
            { instructions =
                [ Nothing ]
            }
    }


test : Level
test =
    { allowed = AllowAll
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 6, y = 5 }
            , Seg { x = 6, y = 5 } { x = 6, y = 6 }
            , Seg { x = 6, y = 6 } { x = 7, y = 6 }
            , Seg { x = 7, y = 6 } { x = 7, y = 7 }
            , Seg { x = 7, y = 7 } { x = 8, y = 7 }
            , Seg { x = 8, y = 7 } { x = 9, y = 7 }
            , Seg { x = 9, y = 7 } { x = 10, y = 7 }
            , Seg { x = 10, y = 7 } { x = 11, y = 7 }
            , Seg { x = 11, y = 7 } { x = 12, y = 7 }
            ]
    , stack = []
    , items =
        [ { x = 6
          , y = 5
          , kind = Node
          }
        , { x = 6
          , y = 6
          , kind = Square
          }
        , { x = 8
          , y = 7
          , kind = Circle
          }
        , { x = 12
          , y = 7
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        selectable []
            [ { instructions =
                    [ Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    ]
              }
            ]
            { instructions =
                [ Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                ]
            }
    }
