(ns universe.todo)

'[universe.units
  (( ) bettern ns semantic)
  (( ) converting a unit-ratio should normalize the converted side to 1)
  (( ) generated convert-unit map
    (( ) chain convertion numbers to permutate every upwards unit k combo)
    (( ) generate downward ratios))
  (( ) unit prose protocol, turn (unit 1.123e7 :s) into a descriptive sentance.
    (( ) use template for specificity steps)
    (( ) rounding)
    "4 months, 6 days, 3 hours, 10 minutes, 13 seconds"
    "123 trillion years"
    "3 billion kilometers")
  (( ) generic domain measures
    (( ) declare base unit per domain)
    (( ) unit type-writes the nearest unit to the value)
    [(12 :y) (3.5 :pc) (1.2145 :km)])]

'[simulation
  (( ) visual-scale - a ratio to apply to gameObjects
    (def VISUAL-SCALE (atom (double 1.2134)))
    (def VISUAL-UNIT (unit 1.2134e10 :Mpc)))
  (( ) fast transform scaling to visual-scale)]