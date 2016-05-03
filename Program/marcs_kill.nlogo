extensions[stats]

breed[workers worker]
breed[problems problem]
breed[datacenters datacenter]

workers-own[answer workers-type penalty penalty-all delay-tick recent-road]
patches-own[true-label road-num]
datacenters-own[list-answer list-workers generate-label each-correctness]

globals
[
  update-ticks
  flood-ticks
  plus-delay-tick
  count-road-num
  grass-color
  correctness
  sum-correctness
  area
  em-list
  hard-list
  kill-workers
  round-cut
  round-cut-type
]

to setup
  file-close
  file-open "file.txt"

  clear-all

  set count-road-num  0
  set grass-color green + 1
  set update-ticks 30
  set flood-ticks 30
  set plus-delay-tick 30
  set area list (area.x)(area.y)
  set round-cut 3
  set round-cut-type 1

   reset-ticks
  
  setup-patches
  setup-datacenters(item 0 area * item 1 area)
  setup-workers

  init-em-list
  set hard-list []

  set kill-workers[]

  toggle-penalty

  ;setup-log

end

to go
  let end-ticks 1440
  if ticks > end-ticks[
    file-close
    stop
   ]

  move

  if ticks mod update-ticks = 0 and ticks > 0
  [
    datacenter-update
  ]


  if ticks mod flood-ticks = 0 and ticks > 0
  [
    setup-flood
  ]

  toggle-penalty
  tick
end

to setup-patches
  ask patches[set pcolor grass-color]

  let seq.x seq(min-pxcor)(max-pxcor)(item 0 area + 1)
  let seq.y seq(min-pycor)(max-pycor)(item 1 area + 1)


  let i 0
  let j 0

  while [ i < (item 0 area)  ][
    while [ j < (item 1 area) ][
      setup-road(item i seq.x)(item (i + 1) seq.x)(item j seq.y)(item (j + 1) seq.y)
      set j j + 1
    ]
    set j 0
    set i i + 1
  ]

  setup-flood
end

to setup-road [xstart xend ystart yend]
  set count-road-num count-road-num + 1

  ask patches
  [
    if pxcor >= xstart and pxcor <= xend and pycor >= ystart and pycor <= yend and pcolor = grass-color
    [
      set pcolor grey
      set true-label 1
      set road-num count-road-num
    ]

  ]
end

to setup-border-x [x]
  ask patches[if pxcor >= x - 1 and pxcor <= x + 1 [set pcolor black]]
end

to setup-border-y [y]
  ask patches[if pycor >= y - 1 and pycor <= y + 1[set pcolor black]]
end

to setup-flood
  random-seed ticks
 
  if problem-label = "flood"
  [
    ask patches
    [
      if road-num >= 1 and road-num <= count-road-num
      [
         set pcolor blue
         set true-label 0
      ]
    ]
  ]
  if problem-label = "random"
  [
    let i 1
    loop[
      let rand random 100
      show rand
      ask patches
      [
        if road-num = i
        [
          ifelse rand < 50
           [set true-label 0]
           [set true-label 1]

          ifelse true-label = 1
          [set pcolor grey]
          [set pcolor blue]
        ]
      ]

      if i = count-road-num [ stop ]
      set i i + 1
    ]
  ]
  if problem-label = "real"
  [
    set flood-ticks 5
    let line split file-read-line " "
    let i 1
    while [i <= 36][
     ask patches
     [
      if road-num = i
      [
        ifelse item (i - 1) line = "-1"
         [set true-label 1]
         [set true-label 0]

         ifelse true-label = 1
          [set pcolor grey]
          [set pcolor blue]
      ]
     ]
     set i i + 1
    ]

  ]
end

to setup-datacenters[datacenters-size]
  ;;change order of number (start at 1)
  create-datacenters 1[
    die
  ]

  create-datacenters datacenters-size
  [
    set size 4
    set shape "house"
    set color grey + 2
    set generate-label "N/A"
    set list-answer []
    set list-workers []
  ]
end

to setup-workers
  let number-adversaries round(adversaries-rate / 100 * number-workers)
  let number-honesties number-workers - number-adversaries

  generate-workers(number-honesties)(0)
  generate-workers(number-adversaries)(adversaries-type)
end

to generate-workers[number w-type]
  create-workers number
  [
    set shape "person"
    set size 1
    setxy random-xcor random-ycor
    set workers-type w-type

    ifelse workers-type = 0
     [set-honest]
     [set-adversaries]

    set penalty 0
    set penalty-all []
  ]
end

to set-honest
  set color white

  create-links-to datacenters[
     set color white
     hide-link
  ]
end

to set-adversaries
  set color black

  create-links-to datacenters[
     set color black
     hide-link
  ]


end

to separate-workers  ;; turtle procedure
  if any? other workers-here
    [ fd 1
      separate-workers ]
end

to move
  ask workers
  [
    forward 1
    workers-turn
    workers-send(who)
  ]
end


to workers-turn
  rt random 50
  lt random 50
end

to workers-send[who-worker]
    ifelse pcolor != grass-color and (recent-road != road-num or delay-tick <= ticks) and ticks > 0
    [
      workers-ans

      let send-ans answer
      ask datacenter road-num[datacenters-recieved(send-ans)(who-worker)]

      set delay-tick ticks + plus-delay-tick
      set recent-road road-num

      ask out-link-to datacenter road-num[show-link]
    ]

    [
      ask my-out-links[hide-link]
    ]

end

to workers-ans
  ifelse color = white
  [normal-ans]
  [adversaries-ans]
end

to normal-ans
  ifelse true-label = 1
  [ifelse random 100 < workers-probability
     [set answer 1]
     [set answer 0]
   ]
  [ifelse random 100 < workers-probability
     [set answer 0]
     [set answer 1]
   ]
end

to adversaries-ans
  if workers-type = "1. fixed : flood"[
    set answer 0
  ]

  if workers-type = "1. fixed : normal"[
    set answer 1
  ]

  if workers-type = "2. random"[
    ifelse random 100 < 50
    [set answer 0]
    [set answer 1]
  ]

  if workers-type = "3. perfect"[
    ifelse true-label = 1
    [ set answer 0 ]
    [ set answer 1]
  ]
end

to datacenters-recieved[send-ans who-worker]
  set list-answer lput send-ans list-answer
  set list-workers lput who-worker list-workers
end

to datacenter-update
 datacenter-fusion
 calculate-correctness
 assign-penalty
 datacenter-reset
end

to datacenter-fusion
   save-hard

   ifelse round-cut-type = 1[
    if use-cut-worker-by-penalty? and floor(ticks / update-ticks) >= start-cut
      [kill-list]
   ]
   [
     if use-cut-worker-by-penalty? and floor(ticks / update-ticks) mod round-cut = 0
      [kill-list]
   ]

  if data-fusion = "Majority Voting"
   [mv]

  if data-fusion = "EM Algorithm"
   [em]
end

to save-hard
  ask datacenters[
   let one-list []
   let zero-list []

   let i 0
   while [i < length list-answer]
   [
      ifelse (item i list-answer) = 1
      [set one-list lput item i list-workers one-list]
      [set zero-list lput item i list-workers zero-list]

      set i i + 1
   ]

   set hard-list lput one-list hard-list
   set hard-list lput zero-list hard-list
  ]
end

to mv
  ask datacenters
  [
    if length list-answer > 0
    [set generate-label round(sum list-answer / length list-answer)]
  ]
end


to weight-mv[parameter]
   let i 0
   let one-list []
   let zero-list []
   while [i < length list-answer]
   [
      ifelse (item i list-answer) = 1
      [set one-list lput item i parameter one-list]
      [set zero-list lput item i parameter zero-list]

      set i i + 1
   ]

   if one-list = []
   [set one-list  [0]]
   if zero-list = []
   [set zero-list [0]]

   let all length one-list + length zero-list

   ifelse (sum one-list / all) > (sum zero-list / all)
   [set generate-label 1]
   [set generate-label 0]
end


to em
   ifelse ticks <= update-ticks[
     mv
   ]
   [
     em-multi
   ]
   save-em-data
end

to em-multi
  let iteration 1000
     let sd 0.001
     let i 0
     let parameter []

    ask datacenters[
       ;;cut worker that send to area
       let em-round-list []
       foreach list-workers[
         set em-round-list lput (item ? em-list) em-round-list
       ]

       ;; set initial parameter
       foreach em-round-list[
         ifelse ? != []
         [set parameter lput mean ? parameter]
         [set parameter lput 0 parameter]
       ]


       ;; check that you can use em?
       ifelse parameter = []
       [mv]
       [
         while [i < iteration][
           let result em-single(em-round-list)(parameter)
           let sd.check 0
           let j 0

           while[j < length result][
             if abs( (item j parameter) - (item j result) ) < sd
             [set sd.check sd.check + 1]
             set j j + 1
           ]

           if sd.check = length result
           [set iteration 0]

           set parameter result

           set i i + 1
         ]

         weight-mv(parameter)
       ]
     ]
end

to-report em-single[em-round-list parameter]
  ;;E-Step
  ;; 1.Find Probability

  let K []
  foreach em-round-list[
   set K lput sum ? K
  ]

  let N[]
  foreach em-round-list[
   set N lput length ? N
  ]

  let prob.t []
  let i 0
  while[i < length N][
    let each.t []
    foreach parameter[
      set each.t lput stats:binomial-probability (item i N) (item i K) ? each.t
    ]

    set prob.t lput each.t prob.t
    set i i + 1
  ]


  ;;2. Normalization
  let normal []
  foreach prob.t[
    let each.normal []
    let sums sum ?
    foreach ?[
      set each.normal lput (? / sums) each.normal
    ]

    set normal lput each.normal normal
  ]

  let weight.t []
  foreach normal[
    let each.weight.t []
    set i 0
    while[i < length K][
      set each.weight.t lput ((item i K) * (item i ?)) each.weight.t
      set i i + 1
    ]

    set weight.t lput each.weight.t weight.t
  ]

  let weight.f []
  foreach normal[
    let each.weight.f []
    set i 0
    while[i < length K][
      set each.weight.f lput (((item i N) - (item i K)) * (item i ?)) each.weight.f
      set i i + 1
    ]

    set weight.f lput each.weight.f weight.f
  ]

  ;;M-Step
  let sum.weight.t []
  foreach weight.t[
    set sum.weight.t lput sum ? sum.weight.t
  ]

  let sum.weight.f []
  foreach weight.f[
    set sum.weight.f lput sum ? sum.weight.f
  ]

  set i 0
  let result []
  while[i < length parameter][
    set result lput ((item i sum.weight.t) / (item i sum.weight.t + item i sum.weight.f)) result
    set i i + 1
  ]

  report result
end

to init-em-list
  set em-list n-values (count datacenters + count workers + 1) [[]]
end

to save-em-data
     ask datacenters[
       let i 0
       while [i < length list-workers][
         let x item (item i list-workers) em-list

         ifelse (item i list-answer) = generate-label
         [set x lput 1 x]
         [set x lput 0 x]

         set em-list replace-item (item i list-workers) em-list x

         set i i + 1
       ]
     ]
end

to datacenter-reset
  ask datacenters
  [
    set list-answer []
    set list-workers []
  ]
end

to toggle-penalty
  ask workers
   [
     set label-color black
     ifelse(show-penalty?)
       [set label precision penalty 2]
       [set label ""]
   ]
end

to calculate-correctness
  let list-correctness []

  ask datacenters
  [
    let dc-number who

    if generate-label != "N/A"[
      let road-label first [true-label] of patches with [road-num = dc-number]

      ifelse generate-label = road-label
      [set each-correctness 100]
      [set each-correctness 0]

      set list-correctness lput each-correctness list-correctness
    ]
  ]

  set correctness mean list-correctness
  set sum-correctness sum-correctness + correctness
end

to assign-penalty
  if reputation-algorithm = "None"
  [set use-cut-worker-by-penalty?  false]

  if reputation-algorithm = "Simple Penalty"
   [simple-penalty]

  if reputation-algorithm = "Soft Penalty"
   [soft-penalty]

  if reputation-algorithm = "Hard Penalty"
   [hard-penalty]
end

to soft-penalty
   ask datacenters[
     let l-answer list-answer
     let round-penalty-zero length filter [? = 0] list-answer
     let round-penalty-one  length filter [? = 1] list-answer
     let i 0

     foreach list-workers [
        ask worker ? [
          if item i l-answer = 1
          [
            if round-penalty-zero >= 1
            [ set penalty-all lput (1 / round-penalty-one) penalty-all
              set penalty (1 / round-penalty-one)
            ]
          ]
          if item i l-answer = 0
          [
            if round-penalty-one >= 1
            [ set penalty-all lput (1 / round-penalty-zero) penalty-all
              set penalty (1 / round-penalty-one)
            ]
          ]
        ]
        set i i + 1
     ]
   ]
end

;; note: still greedy semi-matching
to hard-penalty
   ;; reset and reassign penalty
   ask workers [set penalty 0]

   let result []
   let i 0
   while[ i < length hard-list][
     let index 0
     let edge 0
     let minimum 9999

     foreach item i hard-list[
       let p [penalty] of worker ?
       if p < minimum [
          set minimum p
          set edge list(?)(i)
          set index ?
       ]
     ]

     set result lput edge result
     if index != 0
     [ask worker index [set penalty penalty + 1]]

     set i i + 1
   ]
end

to simple-penalty
   ask datacenters[
     let indexes n-values length list-answer[?]

     foreach indexes[
      if item ? list-answer != generate-label
      [
        ask worker item ? list-workers[set penalty penalty + 1]
      ]
     ]
   ]
end

to cut-list
  ;ask datacenters[show list-workers]

  let list-penalty []

  let i (area.x * area.y) + 1
  while [i <= (area.x * area.y) + number-workers][
    set list-penalty lput [penalty] of worker i list-penalty

    set i i + 1
  ]

   let index-workers-cut sublist order(list-penalty) 0 (round(cut-workers-per-round / 100 * number-workers))

   foreach index-workers-cut [
     ask datacenters[
       let index position (? + count datacenters + 1) list-workers
       if index != false[
        set list-workers remove-item index list-workers
        set list-answer remove-item index list-answer
       ]
     ]
   ]

   ;ask datacenters[show list-penalty]
   ;ask datacenters[show index-workers-cut]
   ;ask datacenters[show list-workers]

end

to kill-list
  let list-penalty []


  ;;cut 0 out

  let i (area.x * area.y) + 1

  ;;pull penalty follow worker id

  while [i <= (area.x * area.y) + number-workers]
  [

    set list-penalty lput [penalty] of worker i list-penalty

    set i i + 1

  ]




  foreach kill-workers
  [
    set list-penalty replace-item ? list-penalty -1
  ]

  let index-worker-top-cut sublist order(list-penalty) 0 (round(cut-workers-per-round / 100 * number-workers))


  set kill-workers sentence index-worker-top-cut kill-workers

  foreach kill-workers
  [

    ask worker (? + count datacenters + 1) [set color red]

    ask datacenters[
       let index position (? + count datacenters + 1) list-workers
       if index != false[
        set list-workers remove-item index list-workers
        set list-answer remove-item index list-answer
       ]
     ]
  ]

end

to-report seq [x y length.out]
    let result []
    let by ((y - x) / (length.out - 1))

    let i 0
    while [i < length.out] [
       set result lput (x + i * by) result
       set i i + 1
    ]
    report result
end

to-report order [x]
  report sort-by [item ?1 x > item ?2 x] shuffle n-values length x[?]
end

to-report split [ string delim ]
  report reduce [
    ifelse-value (?2 = delim)
      [ lput "" ?1 ]
      [ lput word last ?1 ?2 but-last ?1 ]
  ] fput [""] n-values (length string) [ substring string ? (? + 1) ]
end

to setup-log
  output-print "--Setup--"
  output-print ""
end
@#$#@#$#@
GRAPHICS-WINDOW
396
51
816
492
20
20
10.0
1
10
1
1
1
0
1
1
1
-20
20
-20
20
1
1
1
ticks
30.0

BUTTON
125
40
188
73
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
201
40
264
73
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
11
118
183
151
number-workers
number-workers
1
100
100
1
1
NIL
HORIZONTAL

CHOOSER
20
274
158
319
problem-label
problem-label
"flood" "normal" "random" "real"
2

SLIDER
10
159
182
192
workers-probability
workers-probability
50
100
100
1
1
%
HORIZONTAL

SWITCH
1135
109
1271
142
show-penalty?
show-penalty?
1
1
-1000

MONITOR
887
75
1038
120
 correctness of system (%)
sum-correctness / floor(ticks / update-ticks)
4
1
11

SLIDER
9
200
181
233
adversaries-rate
adversaries-rate
0
100
0
1
1
%
HORIZONTAL

CHOOSER
215
189
355
234
adversaries-type
adversaries-type
"1. fixed : flood" "1. fixed : normal" "2. random" "3. perfect"
3

PLOT
858
320
1065
493
correctness per round plot
round
correctness
0.0
48.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks mod update-ticks = 1 and ticks >= update-ticks\n[plot correctness]"

PLOT
852
138
1062
301
average correctness plot
round
average correctness
0.0
48.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks mod update-ticks = 1 and ticks >= update-ticks\n[plot sum-correctness / floor(ticks / update-ticks)]"

MONITOR
1134
50
1189
95
normal
mean [penalty] of workers with [workers-type = 0]
2
1
11

TEXTBOX
913
16
1063
38
Correctness
18
0.0
1

MONITOR
1200
50
1269
95
adversareis
mean [penalty] of workers with [workers-type != 0]
2
1
11

TEXTBOX
1165
16
1315
38
Penalty
18
0.0
1

SLIDER
206
285
349
318
area.x
area.x
1
max-pxcor
1
1
1
NIL
HORIZONTAL

TEXTBOX
1172
197
1202
219
Log
18
0.0
1

OUTPUT
1094
227
1293
449
12

CHOOSER
22
411
160
456
data-fusion
data-fusion
"Majority Voting" "EM Algorithm"
0

TEXTBOX
119
10
311
35
Parameter Setup
20
0.0
1

CHOOSER
23
463
161
508
reputation-algorithm
reputation-algorithm
"None" "Simple Penalty" "Soft Penalty" "Hard Penalty"
2

MONITOR
219
135
269
180
normal
count workers with [workers-type = 0]
17
1
11

MONITOR
281
135
346
180
adversaries
count workers with [workers-type != 0]
17
1
11

TEXTBOX
162
82
312
104
Workers
18
0.0
1

SLIDER
186
462
371
495
cut-workers-per-round
cut-workers-per-round
0
50
1
1
1
%
HORIZONTAL

SLIDER
204
328
351
361
area.y
area.y
1
max-pycor
1
1
1
NIL
HORIZONTAL

CHOOSER
20
328
158
373
label-level
label-level
"2 Levels" "3 Levels"
0

TEXTBOX
161
246
311
268
Problem
18
0.0
1

TEXTBOX
151
384
301
406
Datacenter
18
0.0
1

TEXTBOX
555
13
705
38
Map Output
20
0.0
1

SWITCH
179
420
380
453
use-cut-worker-by-penalty?
use-cut-worker-by-penalty?
0
1
-1000

TEXTBOX
836
39
1118
67
*correctness calculates from\nsum(areas with generate-label = true-label) / all areas
11
0.0
1

TEXTBOX
207
115
378
145
Number of workers per type
12
0.0
1

SLIDER
187
504
359
537
start-cut
start-cut
1
48
40
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

hexagonal prism
false
0
Rectangle -7500403 true true 90 90 210 270
Polygon -1 true false 210 270 255 240 255 60 210 90
Polygon -13345367 true false 90 90 45 60 45 240 90 270
Polygon -11221820 true false 45 60 90 30 210 30 255 60 210 90 90 90

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="number-workers" first="10" step="10" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tests" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="30" step="1" last="70"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test4" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test5soft" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test6" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="10" last="100"/>
    <steppedValueSet variable="cut-workers-per-round" first="0" step="25" last="50"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test7" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test10" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test2.1" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test11" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test12" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;1. fixed : normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test16" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <enumeratedValueSet variable="workers-probability">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test17" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <enumeratedValueSet variable="workers-probability">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="adversaries-rate" first="0" step="20" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="newcut0" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="cut-workers-per-round" first="0" step="1" last="20"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="newcut1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="round-cut" first="0" step="1" last="20"/>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="newcut0plus" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="cut-workers-per-round" first="0" step="1" last="10"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test5hard" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Hard Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test6soft" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test6hard" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;EM Algorithm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Hard Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tests2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tests3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testa3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="40" step="1" last="60"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testa2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="40" step="1" last="60"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testa" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="40" step="1" last="60"/>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="result1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="round-cut">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="round-cut-type">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="result3" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="result2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="area.x" first="1" step="1" last="10"/>
    <steppedValueSet variable="area.y" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="result4" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="result5" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a0" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="50" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;1. fixed : flood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a3.1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;1. fixed : flood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a4" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="60"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a5" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="50" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a2.1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a3.2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;1. fixed : flood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;flood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a6" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="40" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;1. fixed : flood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a7" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="50" step="1" last="70"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b0" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="50" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="c0" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="50" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b4" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="60"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b5" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="50" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b0s" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="workers-probability" first="50" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b1s" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;3. perfect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b2s" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;2. random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="b3s" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="adversaries-rate" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;1. fixed : flood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="a3s_kill" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>sum-correctness / floor(ticks / update-ticks)</metric>
    <steppedValueSet variable="start-cut" first="1" step="1" last="40"/>
    <enumeratedValueSet variable="adversaries-rate">
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-workers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers-probability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adversaries-type">
      <value value="&quot;1. fixed : flood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="problem-label">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="area.y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="label-level">
      <value value="&quot;2 Levels&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-fusion">
      <value value="&quot;Majority Voting&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-algorithm">
      <value value="&quot;Soft Penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-cut-worker-by-penalty?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cut-workers-per-round">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-penalty?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

link1
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 15 90 45
Line -7500403 true 150 15 210 45

@#$#@#$#@
0
@#$#@#$#@
