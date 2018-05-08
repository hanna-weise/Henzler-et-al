extensions [array]
;.............GLOBALS................................
globals
[
  fire?
  time-since-last-fire
  fire-counter
  population-fire
  average-seeds-fire
  average-seeds
  average-seeds-alive
  total-seeds
  total-seeds-fire
  seed-survival
  seedlings-survival1
  seedlings-survival2
  seedlings-survival3
  seedlings-survival4
  seedlings-survival5
  seedlings-survival6
  seedling
  current-weather
  max-seedlings
  recruitment
  recruitment2
  population-change
  mean-fire-interval
  duration
  extinct?
]

patches-own []
turtles-own [age new_seeds produced mortality seeds_plant dead ; variable dead is for soil-seedbank: when they die because of age or mortality, seeds on plant remain until fire event
             seeds_array tempseeds_array] ; arrays for calculation of seed production for alive adults



;............SETUP...................................
to setup
  ca
  setup-patches
  setup-plants
  set fire? false
  set fire-counter 0
  set extinct? false
  set current-weather "average"
  set time-since-last-fire 5
  reset-ticks
end
;....................................................
to setup-patches
  ask patches
  [
    set pcolor 37
  ]
end
;....................................................
to setup-plants
  set-default-shape turtles "tree"
;;  create-turtles 50
  create-turtles 2500
  ask turtles
  [
    set color green
    set dead false
    set age 5                                                                                                           ; at starting point weather condition is average and they start with seed production (younger 5 means they are seedlings)
    set produced false
    set seeds_plant 0
    set new_seeds 0
    set seeds_array array:from-list n-values seed-longevity [0]                                                                     ; empty arrays for seedproduction (seed longevity: 15 yrs. following Groeneveld 2002)
    set tempseeds_array array:from-list n-values seed-longevity [0]
    setxy random-xcor random-ycor
  ]
end
;...................GO......................................
to go
  if ticks >= 200 [stop]

  set time-since-last-fire time-since-last-fire + 1
  ; counter for time-since-last-fire
  if fixed-fire-scenario = false and time-since-last-fire >= 5 ;; doesn´t go in there in the behavior space experiment ["fixed-fire-scenario" true]
  [
    if probability-fire-scenario = true [burn-pf]
  ]
  if frequent-fire = false [demographic-envelope] ; determines whether there will be a fire, only one fire for de, fire processes calculated here
  if frequent-fire = true [interval-squeeze] ; determines whether there will be a fire, multiple fires for IS, fire processes calculated here
  weather-condition
  calculate-seeds                                                                                                   ; probability for dry/average year
  inter-fire-survival-adults
  establishment
  inter-fire-survival-seedlings
  post-fire-recruitment                                                                                                 ; probability of recruited seedlings from seeds
  post-fire-recruitment2
  population-changing                                                                                                   ; calculate population-change
  survival-duration
  tick
  if any? turtles or seedling > 0 [set duration ticks]
end



;.......................... DIFFERENT FIRE SCENARIOS ....................................................
;burn for fire-probability
to burn-pf
  if random-float 1 < fire-probability
  [
    set population-fire 0
    set population-fire (population-fire + count turtles with [dead = false] )                                          ; size of population when burning
    set seedling 0                                                                                                      ; seedlings and seeds die
    calculate-average-seeds-fire
    dispersal
    ask turtles                                                                                                         ; local population
    [                                                                                                        ; dispersal and germination when fire
      die
    ]
    set time-since-last-fire 0
    set fire-counter fire-counter + 1
  ]
end

;..............................................................................
;burn for fixed-fire-scenario
;one fire
to demographic-envelope
;;  if fixed-fire-scenario = true and ticks = deterministic-fire-interval                                                 ; current fire interval: 13 yrs follwing Esther 2008
if fixed-fire-scenario = true and time-since-last-fire = deterministic-fire-interval and ticks <= ((deterministic-fire-interval - 5) + deterministic-fire-interval)  ; the last condition is to ensure that there are only two fires
  [
    set population-fire 0
    set population-fire (population-fire + count turtles with [dead = false])                                           ; size of population when burning
    set seedling 0                                                                                                      ; seedlings and seeds die
    calculate-average-seeds-fire
    calculate-total-seeds-fire
    dispersal
    ask turtles                                                                                                         ; local population
    [                                                                                                              ; dispersal and germination when fire
      die
    ]
    set time-since-last-fire 0
    set fire-counter fire-counter + 1
  ]
end


;..............................................................................

;frequent fire
to interval-squeeze
;;  if fixed-fire-scenario = true and ticks > 0 and ticks mod deterministic-fire-interval = 0                             ; frequent fire by fire-interval
if fixed-fire-scenario = true and time-since-last-fire = deterministic-fire-interval  ;; -----> changed HW
  [
    set population-fire 0
    set population-fire (population-fire + count turtles with [dead = false])                                           ; size of population when burning
    set seedling 0                                                                                                      ; seedlings and seeds die
    calculate-average-seeds-fire
    dispersal                                                                                                           ; dispersal and germination when fire
    ask turtles                                                                                                         ; local population
    [
      die
    ]
    set time-since-last-fire 0
    set fire-counter fire-counter + 1
  ]
end


;..............................................................................
; for dispersal seeds are stored in gloabl variable: seedling
to dispersal
  ;set survival probabilities for seeds
  ifelse current-weather = "average"
  [;average
    ifelse seedbank = "canopy"
    [set seed-survival (ger-canopy * surv0-av)]
    [set seed-survival (ger-soil * surv0-av)]
  ]
  [;dry
    ifelse seedbank = "canopy"
    [set seed-survival (ger-canopy * surv0-dry)]
    [set seed-survival (ger-soil * surv0-dry)]
  ]
  ;calculation of survival probability for all seeds
  set seedling 0
  set seedling (binomial total-seeds seed-survival)
end



;...........................calculate_seeds.....................................
to calculate-seeds
  ;serotinous and SSB non-sprouters have same seed production
  ifelse current-weather = "dry"
  ;when weather condition of previous year is dry plants start seed production later (up to 2 years delay)
  [
;    if any? turtles with [dead = false]
;    [
    ask turtles with [age >= (age-mat-dry + 1) and dead = false or dead = false and produced = true]              ;or produced = true                                                              ; only alive turtles (dead = false) produce seeds
    [
      let reduced_production seedprod-dry                                                                                           ;-> value: Enright 2015: demographic shift (reduced seed production)
      set new_seeds (reduced_production * (x * exp(- k * age)) / (1 + 90 * exp(- k * age)) ^ 2)                      ; following Enright et al 1996 and Groeneveld 2002
      ;set a new_seeds
      array:set seeds_array 0 new_seeds                                                                                    ; array for seeds: 1. position new_seeds
      foreach n-values (seed-longevity - 1) [ ?1 -> ?1 ] [ ?1 -> array:set seeds_array ?1 + 1 array:item tempseeds_array ?1 ]                ; copy of tempseeds to seeds from position 2-15
      foreach n-values seed-longevity [ ?1 -> ?1 ] [ ?1 -> array:set tempseeds_array ?1 array:item seeds_array ?1 ]                    ; copy of array seeds to tempseeds for next tick
      set seeds_plant 0                                                                                                    ; seeds stored on plant
      foreach n-values seed-longevity [ ?1 -> ?1 ] [ ?1 -> set seeds_plant seeds_plant + array:item seeds_array ?1 ]                   ; total of array seeds
      ;set produced true                                                                                                    ; if plant once started with seed production it will produce until dead                                                                                                                ;      print (word "array-seeds " seeds)
      ;print (word "array-tempseeds " tempseeds)
      ;print (word "sum of array-seeds " seeds_plant)
    ]
  ]
  ;when weather condition of previous year is average plants start seed production with 5 years
  [
    ask turtles with [age >= (age-mat1-av + 1) and dead = false]
    [
      let reduced_production seedprod-av
      set new_seeds (reduced_production * (x * exp(- k * age)) / (1 + 90 * exp(- k * age)) ^ 2)
      ;set a new_seeds
      array:set seeds_array 0 new_seeds
      foreach n-values (seed-longevity - 1) [ ?1 -> ?1 ] [ ?1 -> array:set seeds_array ?1 + 1 array:item tempseeds_array ?1 ]
      foreach n-values seed-longevity [ ?1 -> ?1 ] [ ?1 -> array:set tempseeds_array ?1 array:item seeds_array ?1 ]
      set seeds_plant 0
      foreach n-values seed-longevity [ ?1 -> ?1 ] [ ?1 -> set seeds_plant seeds_plant + array:item seeds_array ?1 ]
      set produced true
      ;print (word "array-seeds " seeds)
      ;print (word "array-tempseeds " tempseeds)
      ;print (word "sum of array-seeds " seeds_plant)
    ]
  ]
  ; seed storage for dead adults of soil-seedbank non-sprouters
  ask turtles with [dead = true]
  [
    ;set new_seeds 0
    array:set seeds_array 0 0
    foreach n-values (seed-longevity - 1) [ ?1 -> ?1 ] [ ?1 -> array:set seeds_array ?1 + 1 array:item tempseeds_array ?1 ]
    foreach n-values seed-longevity [ ?1 -> ?1 ] [ ?1 -> array:set tempseeds_array ?1 array:item seeds_array ?1 ]
    set seeds_plant 0
    foreach n-values seed-longevity [ ?1 -> ?1 ] [ ?1 -> set seeds_plant seeds_plant + array:item seeds_array ?1 ]
    ;set produced true
    ;show seeds_plant
    ;show seeds_array
  ]
  if any? turtles
  [
    set average-seeds 0
    set average-seeds (average-seeds + mean [seeds_plant] of turtles )                                                     ; average-seeds: average number of stored seeds
    set total-seeds 0
    set total-seeds (total-seeds + sum [seeds_plant] of turtles)                                                           ; total-seeds: all stored seeds on landscape
  ]
  if not any? turtles;count turtles <= 0 ;and seedling <= 0
  [
    set average-seeds 0                                                                                                    ; if all adult dead, no more seeds available
    set total-seeds 0
  ]
  if any? turtles with [dead = false]
  [
  set average-seeds-alive 0
  set average-seeds-alive (average-seeds-alive + mean [seeds_plant] of turtles with [dead = false])
    ]
  if not any? turtles with [dead = false]
  [
   set average-seeds-alive 0
  ]
  ;ask turtles [show seeds_plant]
end



;.......................weather-condition..........................................
;determine if this is a dry or an average year
to weather-condition                                                                                                       ; actual: pdy = 0.29, cc45: pdy = 0.42, cc85: pdy = 0.69
  ifelse random-float 1 < dry-year-probability                                                                             ; weather-condition of current year influences inter-fire survival of seedlings and adults (water-availability)
    [set current-weather "dry"]
  [set current-weather "average"]
end



;...........................plant age and mortality...............................
;plants own age and die
to inter-fire-survival-adults
  ask turtles with [dead = false]
  [
    ;let increased_mortality 1
    ;if current-weather = "dry" [set increased_mortality mort-dry]                                                              ; -> value: Enright 2015, increased mortality during drought
    set age (age + 1 )
    ifelse age > 25
    [
      ifelse current-weather = "average"
      [set mortality mort>25-av]                                                                ; -> value: Groeneveld 2002, mortality caused by age and eventually drought
      [set mortality mort>25-dry]
    ]
    [
      ifelse current-weather = "average"
      [set mortality mort<25-av]
      [set mortality mort<25-dry]
    ]

    ; normal mortality
    if random-float 1 < mortality
    [
      ifelse seedbank = "canopy"
      [;if serotinous die, seeds dont remain
        set seeds_plant 0
        set new_seeds 0
        die
      ]
      [; if SSB die, seeds remain
        set new_seeds 0
        set produced false
        set dead true
      ]
    ]
    if age > max-age
    [
      ifelse seedbank = "canopy"
      [
        set seeds_plant 0
        set new_seeds 0
        die
      ]
      [
        set new_seeds 0
        set produced false
        set dead true
      ]
    ]
  ]
end



;...........................seedlings (inter-fire survival, competition, establishment, recruitment)........................
;seedlings-survival
to inter-fire-survival-seedlings
  ifelse current-weather = "average"
    [;average
      set seedlings-survival1 surv1-av                                                                                           ;survival probabilites for seedlings (1 year - 6 years)
      set seedlings-survival2 surv2-av
      set seedlings-survival3 surv3-av
      set seedlings-survival4 surv4-av
      set seedlings-survival5 surv5-av
      set seedlings-survival6 surv6-av
  ]
  [;dry
    set seedlings-survival1 surv1-dry                                                                                             ;survival probabilites for seedlings (1 year - 6 years)
    set seedlings-survival2 surv2-dry
    set seedlings-survival3 surv3-dry
    set seedlings-survival4 surv4-dry
    set seedlings-survival5 surv5-dry
    set seedlings-survival6 surv6-dry
  ]
  ;seedling1 (1 year old) -> first year after fire
  if time-since-last-fire = 1 and seedling > 0
  [set seedling (binomial seedling seedlings-survival1)
   seedlings-competition]
  ;seedling2 (2 years old) -> second year after fire
  if time-since-last-fire = 2 and seedling > 0
  [set seedling (binomial seedling seedlings-survival2)
   seedlings-competition]
  ; seedling3 (3 years old) -> third year after fire
  if time-since-last-fire = 3 and seedling > 0
  [set seedling (binomial seedling seedlings-survival3)
   seedlings-competition]
  ;seedling4 (4 years old) -> fourth year after fire
  if time-since-last-fire = 4 and seedling > 0
  [set seedling (binomial seedling seedlings-survival4)
   seedlings-competition]
  ;seedling5 (5 years old) -> fifth year after fire
  if time-since-last-fire = 5 and seedling > 0
  [set seedling (binomial seedling seedlings-survival5)
   seedlings-competition]
  ;seedling6 (6 years old) -> sixth year after fire
  if time-since-last-fire = 6 and seedling > 0
  [set seedling (binomial seedling seedlings-survival6)
   seedlings-competition]
end



; intraspecific seedling-competition for large-sized seeds
to seedlings-competition
  set max-seedlings 0
  set max-seedlings (max-seedlings + seedling ^ 1)                                                                              ; intraspecific seedling competition for large sized seeds (Esther 2008)
  if seedling > 0 and seedling >= max-seedlings                                                                                 ; if number of seedlings exceeds capicity of seedlings
    [
      set seedling max-seedlings
  ]
end



;seedlings become adults
to  establishment
  ifelse current-weather = "average"
  [
    if time-since-last-fire >= age-mat1-av and fire-counter >= 1                                                                           ; if average than up to 5 year old seedlings become adults
    [
      if seedling > count patches
      [set seedling count patches]
      ask n-of seedling patches
      [
        if count turtles-here <= 0                                                                                              ; seedlings can become adults when patch isnt occupied by adult
        [
          sprout 1                                                                                                              ; one seedling out of all on one patch becomes adult, others die -> thinning out: competition
          [
            set color green
            set dead false
            set age (age-mat1-av + 1)
            set new_seeds 0
            set seeds_plant 0
            set seeds_array array:from-list n-values seed-longevity [0]                                                                      ; empty arrays for seeds production
            set tempseeds_array array:from-list n-values seed-longevity [0]
            set produced false
          ]
          set seedling 0                                                                                                         ; after establishment no more seedlings available
        ]
        set seedling 0                                                                                                           ; no seedlings available because patch was occupied by adults
      ]
    ]
  ]
  [
    if time-since-last-fire >= age-mat-dry and fire-counter >= 1                                                                             ;when seedlings are 7 years old they become adult independently of weather-condition
    [
      if seedling > count patches
      [set seedling count patches]
      ask n-of seedling patches
      [
        if count turtles-here <= 0
        [
          sprout 1
          [
            set color green
            set dead false
            set age (age-mat-dry + 1)
            set new_seeds 0
            set seeds_plant 0
            set seeds_array array:from-list n-values seed-longevity [0]
            set tempseeds_array array:from-list n-values seed-longevity [0]
            set produced false
          ]
          set seedling 0
        ]
        set seedling 0
      ]
    ]
  ]
end


; post-fire-recruitment shift: probability for recruitment as ratio of seedlings per seeds
; average-seeds
to calculate-average-seeds-fire
  if any? turtles
    [
      set average-seeds-fire 0
      set average-seeds-fire (average-seeds-fire + mean [seeds_plant] of turtles)                                                   ;calculate average seeds over all dying adults
    ]
end
;--------------------------------------------------------
to calculate-total-seeds-fire
  if any? turtles
    [
      set total-seeds-fire 0
      set total-seeds-fire sum[seeds_plant] of turtles
    ]
end
;--------------------------------------------------------
to post-fire-recruitment
    if time-since-last-fire = 3 and fire-counter >= 1 and average-seeds-fire > 0
    [
      set recruitment ((seedling / average-seeds-fire)/ 100)
    ]
end
;--------------------------------------------------------
to post-fire-recruitment2
    if time-since-last-fire = 3 and fire-counter >= 1 and total-seeds-fire > 0
    [
      set recruitment2 (seedling / total-seeds-fire)
    ]
end
;--------------------------------------------------------

;...........................self-replacement....................................
to population-changing
  if any? turtles with [dead = false] and fire-counter >= 1 and population-fire > 0
  [
    set population-change 0
    set population-change (count turtles with [dead = false] / population-fire)
  ]
  if not any? turtles with [dead = false]
  [
    set population-change 0
  ]
end



;...........................calculate-mean-fire-interval...............................
to calculate-mean-fire-interval
  ifelse fire-counter > 0
    [set mean-fire-interval 200 / fire-counter]
  [set mean-fire-interval "NA"]
end



;...........................survival-duration..........................................
to survival-duration
  if not any? turtles and seedling = 0
  [set extinct? true]
end



;..........................binomial distribution....................................................
to-report binomial [n p]
  report length filter [ ?1 -> ?1 < p ] n-values n [random-float 1]
end
@#$#@#$#@
GRAPHICS-WINDOW
728
75
935
283
-1
-1
3.98
1
10
1
1
1
0
1
1
1
0
49
0
49
0
0
1
ticks
30.0

BUTTON
9
10
75
43
NIL
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

SWITCH
9
135
174
168
probability-fire-scenario
probability-fire-scenario
1
1
-1000

SLIDER
222
104
409
137
fire-probability
fire-probability
0
1
0.5
0.1
1
NIL
HORIZONTAL

BUTTON
83
10
146
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
10
50
73
83
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
431
364
716
494
number of adult plants
time
abundance
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [dead = false]"

PLOT
420
207
717
355
number of seedlings
time
abundance
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Seedlings" 1.0 0 -13840069 true "" "plot seedling"

MONITOR
143
250
246
295
NIL
time-since-last-fire
17
1
11

SLIDER
222
144
408
177
dry-year-probability
dry-year-probability
0.00
1
0.69
0.01
1
NIL
HORIZONTAL

MONITOR
62
249
136
294
NIL
fire-counter
17
1
11

MONITOR
8
182
112
227
NIL
current-weather
17
1
11

MONITOR
4
249
54
294
NIL
fire?
17
1
11

MONITOR
4
398
67
443
Seedlings
seedling
0
1
11

MONITOR
116
182
178
227
NIL
extinct?
17
1
11

PLOT
420
10
717
200
Seedproduction
time
abundance
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"new seeds" 1.0 0 -7500403 true "" "ask turtles [plot new_seeds]"
"seeds plant" 1.0 0 -2674135 true "" "ask turtles [plot seeds_plant]"
"pen-2" 1.0 0 -955883 true "" "plot average-seeds-alive"

MONITOR
183
182
249
227
NIL
duration
17
1
11

MONITOR
4
328
94
373
average seeds 
average-seeds
1
1
11

SWITCH
8
95
174
128
fixed-fire-scenario
fixed-fire-scenario
0
1
-1000

MONITOR
170
464
255
509
NIL
population-fire
17
1
11

MONITOR
4
464
77
509
adults
count turtles with [dead = false]
17
1
11

SLIDER
221
64
409
97
deterministic-fire-interval
deterministic-fire-interval
0
100
13.0
0.1
1
NIL
HORIZONTAL

SWITCH
82
47
209
80
frequent-fire
frequent-fire
1
1
-1000

MONITOR
262
464
362
509
NIL
population-change
1
1
11

MONITOR
99
327
209
372
NIL
average-seeds-fire
1
1
11

MONITOR
252
250
364
295
NIL
mean-fire-interval
17
1
11

TEXTBOX
6
447
156
465
Adults
11
0.0
1

TEXTBOX
7
380
157
398
Seedlings
11
0.0
1

TEXTBOX
7
233
157
251
Fire
11
0.0
1

TEXTBOX
7
311
157
329
Seeds
11
0.0
1

CHOOSER
234
10
372
55
seedbank
seedbank
"canopy" "soil"
0

MONITOR
86
464
164
509
dead adults
count turtles with [dead = true]
17
1
11

MONITOR
214
327
325
372
NIL
average-seeds-alive
1
1
11

SLIDER
3
537
175
570
max-age
max-age
0
100
40.0
1
1
NIL
HORIZONTAL

SLIDER
550
538
722
571
age-mat1-av
age-mat1-av
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
550
612
722
645
age-mat-dry
age-mat-dry
0
100
6.0
1
1
NIL
HORIZONTAL

SLIDER
550
574
722
607
age-mat2-av
age-mat2-av
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
725
537
897
570
mort<25-av
mort<25-av
0
100
0.023
1
1
NIL
HORIZONTAL

SLIDER
727
610
899
643
mort<25-dry
mort<25-dry
0
100
0.024
1
1
NIL
HORIZONTAL

SLIDER
726
574
898
607
mort>25-av
mort>25-av
0
100
0.033
1
1
NIL
HORIZONTAL

SLIDER
3
643
175
676
ger-canopy
ger-canopy
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
4
682
176
715
ger-soil
ger-soil
0
100
0.8
1
1
NIL
HORIZONTAL

SLIDER
183
538
355
571
surv0-av
surv0-av
0
100
0.04
1
1
NIL
HORIZONTAL

SLIDER
359
537
531
570
surv0-dry
surv0-dry
0
100
0.03
1
1
NIL
HORIZONTAL

SLIDER
183
573
355
606
surv1-av
surv1-av
0
100
0.53
1
1
NIL
HORIZONTAL

SLIDER
360
572
532
605
surv1-dry
surv1-dry
0
100
0.31
1
1
NIL
HORIZONTAL

SLIDER
183
610
355
643
surv2-av
surv2-av
0
100
0.78
1
1
NIL
HORIZONTAL

SLIDER
359
607
531
640
surv2-dry
surv2-dry
0
100
0.46
1
1
NIL
HORIZONTAL

SLIDER
182
646
354
679
surv3-av
surv3-av
0
100
0.88
1
1
NIL
HORIZONTAL

SLIDER
359
646
531
679
surv3-dry
surv3-dry
0
100
0.5
1
1
NIL
HORIZONTAL

SLIDER
182
683
354
716
surv4-av
surv4-av
0
100
0.91
1
1
NIL
HORIZONTAL

SLIDER
359
683
531
716
surv4-dry
surv4-dry
0
100
0.51
1
1
NIL
HORIZONTAL

SLIDER
181
717
353
750
surv5-av
surv5-av
0
100
0.93
1
1
NIL
HORIZONTAL

SLIDER
358
718
530
751
surv5-dry
surv5-dry
0
100
0.51
1
1
NIL
HORIZONTAL

SLIDER
182
754
354
787
surv6-av
surv6-av
0
100
0.94
1
1
NIL
HORIZONTAL

SLIDER
358
753
530
786
surv6-dry
surv6-dry
0
100
2.0
1
1
NIL
HORIZONTAL

SLIDER
5
753
177
786
seedprod-dry
seedprod-dry
0
100
0.55
1
1
NIL
HORIZONTAL

SLIDER
5
717
177
750
seedprod-av
seedprod-av
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
3
571
175
604
seed-longevity
seed-longevity
0
100
15.0
1
1
NIL
HORIZONTAL

SLIDER
547
704
719
737
x
x
0
100
27000.0
1
1
NIL
HORIZONTAL

SLIDER
3
607
175
640
k
k
0
100
0.4
1
1
NIL
HORIZONTAL

SLIDER
726
649
898
682
mort>25-dry
mort>25-dry
0
100
0.035
1
1
NIL
HORIZONTAL

MONITOR
330
325
396
370
NIL
total-seeds
0
1
11

@#$#@#$#@
## GENERAL INFORMATION
Julia Henzler, Hanna Weise, Neal J Enright, Susanne Zander, Britta Tietjen

A squeeze in the suitable fire interval: simulating the future persistence of fire-killed plants in a Mediterranean-type ecosystem


## PURPOSE

The purpose of this model is to investigate the population dynamics of Banksia hookeriana, a shrub that occurs in the northern sandplains of South-West Australia. The plant represents a plant strategy that is very common in Mediterranean type ecosystems: individuals are killed in the regular fires and the population persists through a seedbank. B. hookeriana is highly serotinous, that is, plants store their seeds in the canopy and require fire cues to germinate. According to climate change projections the conditions in this area will become hotter and drier and fire intervals will become shorter (Moritz et al., 2012). We use our model to understand the effects of the change in both, fire and rain conditions on the plant´s population persistence.

## ENTITIES, STATE VARIABLES AND SCALES

The model entities are individual mature B. hookeriana plants. They are characterized by their age and the number of seeds in their canopy. The seedlings are not modelled as individual plants, that is total number of seedlings from a population is calculated using information on the mortalities at specific ages.   
The model runs in annual time-steps for a simulation period of 200 years. It is not spatially explicit. We are modelling a local population in a landscape. We therefore assume an upper limit of 2500 adult plants. 


## PROCESS OVERVIEW AND SCHEDULING

An overview of the processes and their scheduling can be found in the main text (Fig 1). 
We used fixed, non-random fire intervals to assess the effects of changing fire frequencies systematically. Thus, we record the years since last the fire (time-since-fire) and determine at the beginning of the simulation, if the next year will be a fire year. At the beginning of the year, we determine the weather conditions. For this, we use a random number generator and evaluate whether it is an average or a dry year. The plant processes differ between fire and inter-fire years. 


Seed dispersal (fire years): 
The number of seeds over all plants in summed up. Our model is not spatially explicit thus seeds are not dispersed in a landscape. 

Fire mortality (fire years): 
B. hookeriana lacks the ability to resprout (Enright and Lamont, 1989) thus all plants die in a fire. 

Germination (fire years): 
We assume that although all seeds germinate in the winter following a fire, a high mortality of the seedlings takes place (Enright et al., 1998), further details see sub model seedling survival). 

Produce seeds (inter-fire years): 
Adult plants produce new seeds. These are added to the canopy seedbank. Seed production increases with age, until it reaches the maximum level. Less seeds are produced in dry years. 

Inter-fire survival adults (inter-fire years): 
Each plant has a risk to die, depending on the plants age and the weather conditions. When the adult plant dies all seeds in its canopy die as well. Plant mortality is drawn in a randomized order. 

Inter-fire survival seedlings (inter-fire years): 
The mortality of all seedlings in the population is calculated for the whole population by drawing the number of surviving seedlings from a binomial distribution. The mortalities depend on the age of the seedlings and on the weather conditions.  
 
Establishment (inter-fire years): 
Seedlings turn into mature plants after a specific period. It depends on the weather conditions at which age they mature: the earliest time is five years after a fire. However, if this year is dry, maturity is delayed. The delay can last up to two years, depending on the weather conditions in the subsequent years.   


## HOW TO USE IT

1. Switch fire-Scenario to ON setzten, if you want to simulate fire
2. Choose probably of dry year and of fire
3. Push Setup-button
4. Run simulation with Go-button
5. Observe: how does population respond?


## RELATED MODELS

Enright, N. J., Lamont, B.B., Marcula R., 1996, Canopy seed bank dynamics and optimum fire regims for the highly serotinous shrub Banksia hookeriana,Journal of Ecology, Vol. 84, pp 9-17

Enright, N. J., Marsula, R., Lamont, B. B., Wissel, C., 1998, The ccological
significance of canopy seed storage in fire-prone environments: a model for
non-sprouting shrubs, Journal of Ecology, Vol. 86, pp 946-959

Esther, A., Groeneveld, J., Enright, N. J., Miller, B. P., Lamont, B.B., Perry G. L. W.,
Schnurr, F. M. Jeltsch, F., 2008, Assessing the importance of seed immigration
on coexistence of plant fuctional types in a species-rich ecosystem, Ecological
Modelling, Vol. 213, pp 402-416

Groeneveld, J., Enright, N. J., Lamont, B. B., Wissel, C., 2002, A spatial model of
coexistence among three Banksia species along a topographic gradient in fire-
prone shrubland, Journal of Ecology, Vol. 90, pp 762-774


## CREDITS AND REFERENCES

Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
@#$#@#$#@
0
@#$#@#$#@
