;; WATERING_IRRIGATION_RBB: Please refer the info tab for further information and suggestions for application. Please email Dr Kavin Narasimhan (k.narasimhan@surrey.ac.uk) for comments/queries

extensions [ rnd ]

globals
[
  year_number                               ;; counter to keep track of model years
  month_number                              ;; 0 - 11
  low_flow                                  ;; annual water flow in river is low
  medium_flow                               ;; annual water flow into the river is medium
  high_flow                                 ;; annual water flow into the river is high
  annual_flow                               ;; annual water flow in the simulated river
  water_source                              ;; amount of water at source, which provides to an irrigation scheme
  water_scheme                              ;; amount of water allocated to the scheme (might be stored in a reservoir)
  main_canal_level                          ;; pycor of the main canal
  sec_canal_level                           ;; pycor of the starting point of secondary canals (one patch below main canal)
  wat_allocated_irr                         ;; volume of water at source allocated for irrigation (equivalent of MCM units)
  scheme_peak                               ;; lowest depression among patches
  scheme_valley                             ;; highest depression among patches
  scheme_spread                             ;; difference between scheme_valley and scheme_peak
  scheme_middle                             ;; mean of the highest and lowest depression of patches
  scheme_depth                              ;; maximum depression or greatest depth of patches
  scheme_valley_middle                      ;; scheme valley middle line, used for colouring the landscape
  scheme_peak_middle                        ;; scheme peak middle line, used for colouring the landscape
  scheme_meander                            ;; water level of patches
  scheme_steepness                          ;; slope of patches
  hourly_water_supply                       ;; amount of water discharged from reservoir each irrigation hour
  monthly_water_supply                      ;; amount of water discharged from reservoir each month
  monthly_water_use                         ;; amount of water drawn from reservoir each month
]

patches-own [
  p_type                                   ;; reservoir/buffer/mcanal/scanal/valley
  surface_level                            ;; surface level of patch
  water_level                              ;; water level of patch; will be the same as surfaceLevel when patch is dry. can never be less than surfaceLevel
  water_vol                                ;; volume of water on patch (waterLevel - surfaceLevel)
]

to setup
  ca
  initialise-globals
  create-water
  create-irrigation-system
  reset-ticks
end

to go
  irrigate
  update-time
  tick                                            ;; each tick signifies one simulated month
end

;; initialise variables controlling simulated time
to initialise-globals
  set year_number 1
  set month_number 0                                                             ;; month number starts at 0 to work easily with NetLogo lists
  set main_canal_level 20
  set sec_canal_level main_canal_level - 1
  set wat_allocated_irr 0
  set scheme_meander 10                                                         ;; controls the depression of patches
  set scheme_steepness 5                                                        ;; controls the slope of patches
end

;; create water in the scheme
to create-water
  set low_flow []
  set medium_flow []
  set high_flow []
  let low_elevation []
  let med_elevation []
  let high_elevation []
  let wl_counter 0

  ;; assumption: water level at source (which supplies to a reservoir in the irrigation scheme) follows a cosine function
  ;; assumption: water level would be average on most years, and really low or quite high in some years
  ;; implementation of assumptions
  ;; step #1: calculate values for simulated annual low, mean and high water levels (elevations)
  repeat 12 [
    set low_elevation lput precision (minRElevation + (random-float 10 * cos(wl_counter * 45))) 2 low_elevation
    set med_elevation lput precision (meanRElevation + (random-float 10 * cos(wl_counter * 45))) 2 med_elevation
    set high_elevation lput precision (maxRElevation + (random-float 10 * cos(wl_counter * 45))) 2 high_elevation
    set wl_counter wl_counter + 1 ]
  ;; step #2: find the (closest) volume corresponding to the simulated elevations
  foreach low_elevation [ x ->
    set low_flow lput item (position (closest-value x read-from-string rElevation) read-from-string rElevation) read-from-string rVolume low_flow ]
  foreach med_elevation [ x ->
    set medium_flow lput item (position (closest-value x read-from-string rElevation) read-from-string rElevation) read-from-string rVolume medium_flow ]
  foreach high_elevation [ x ->
    set high_flow lput item (position (closest-value x read-from-string rElevation) read-from-string rElevation) read-from-string rVolume high_flow ]
  ;; step #3: choose an elevation-volume combination (low,mean or high) to determine the water at source at the start of a year
  set annual_flow (list (list low_flow .1 ) (list medium_flow .7 ) (list high_flow .05 ) ) ;; choose water availability for the new year (low/mean/high) based on specified probabilities
  set water_source first rnd:weighted-one-of-list annual_flow [ [p] -> last p ]            ;; assign water level at source for the new year based on annual_flow
end

;; create the irrigation system
to create-irrigation-system
  ;; create reservoir
  ask patches with [pxcor < -18 and pycor > 20 ] [ set pcolor blue set p_type "reservoir" ask patch -20 24 [ set plabel "RESERVOIR" ] ]
  ;; create a buffer (dummy) zone
  ask patches with [pxcor >= -18 and pycor > 20] [ set pcolor grey set p_type "buffer" ]
  ;; create the main canal
  ask patches with [ pycor = main_canal_level ] [ set pcolor blue set p_type "mcanal" ask patch 0 main_canal_level [ set plabel "MAIN CANAL"] ]
  ;; create the secondary canals
  ask patches with [ pxcor mod 15 = 0 and pycor < main_canal_level ] [ set pcolor blue set p_type "scanal" ]
  ;; calculate water_level of patches that are not reservoir, buffer, mcanal and scanal based on the impress-variation method
  ask patches with [ p_type = 0 ][ impress-variation ]
  ;; assume that waterlevel of reservoir, buffer, mcanal and scanal patches is 1 higher than other patches
  ask patches with [ p_type != 0 ] [ set water_level (1 +  max [ water_level ] of patches with [ p_type = 0 ]) ]
  ;; calculate surface level of patches based on average elevation of the target area (surface_elevation in masl set via Interface) + water level of patches calculated above + related peak, valley and spread measures calculated below
  set scheme_peak max [ water_level ] of patches with [ p_type = 0 ]
  set scheme_valley min [ water_level ] of patches with [ p_type = 0 ]
  if scheme_peak = scheme_valley [
  set scheme_peak scheme_peak + 2
  set scheme_valley scheme_valley - 2 ]
  set scheme_spread scheme_peak - scheme_valley
  ask patches [ set surface_level (surface_elevation + (water_level - scheme_peak) / scheme_spread * max-pxcor) ]
  ;; tilt the landscape so everything runs downhill from North to South
  dry-setup
  ;; slope set by steepness
  ask patches [ set surface_level (surface_level + ( pycor / max-pycor ) * scheme_spread * scheme_steepness * .02) ]
  dry-setup
end

to impress-variation
  let px% 0
  let py% 0
  let sweep_width 0
  let meander_frequency 0
  let adj_py% 0
  let nearest_canal_point 0
  let xcor_nearest_canal_point 0
  set nearest_canal_point min-one-of patches with [ p_type = "scanal" ] [ distance myself ]
  set xcor_nearest_canal_point [ pxcor ] of nearest_canal_point
  ;; patches closest to the canal (scanals) have greater elevation
  set px% 100 - abs (pxcor - xcor_nearest_canal_point)
  set py% 1 - (min-pycor - pycor) / world-height     ;; results in values 0 ..1
  set sweep_width .01 * scheme_meander               ;; changing scheme_meander or the multiplication factor changes the smoothness of the distribution of elevation of patches up-down and left-right of the scanals
  set meander_frequency px% * 15                     ;; allows patches closest to scanals to have higher elevation and therefore darker patch colours
  set adj_py% (py% + (sweep_width * sin (meander_frequency)))
  set water_level abs adj_py%                        ;; set water level of patches
end

;; reset water level to surface level, i.e., dry the landscape
to dry-setup
  ask patches [ set water_level surface_level ]
  calc-variation
  update-setup
end

;; calculate peak, valley and spread values based on surface level measures: useful to model surface runoff later on if desired
to calc-variation
  set scheme_valley max [ surface_level ] of patches with [ p_type = 0 ]
  set scheme_peak min [ surface_level ] of patches with [ p_type = 0 ]
  if scheme_valley = scheme_peak [
    set scheme_valley scheme_valley + 2
    set scheme_peak scheme_peak - 2 ]
  set scheme_middle ( scheme_valley + scheme_peak ) * 0.5
  set scheme_spread abs ( scheme_valley - scheme_peak )
  set scheme_valley_middle (scheme_valley + scheme_middle) * 0.5
  set scheme_peak_middle (scheme_peak + scheme_middle) * 0.5
  set scheme_depth scheme_valley
end

;; apply colour to patches
to update-setup
  no-display         ;; freeze display when updating
  colour-setup       ;; apply colour
  display            ;; refresh-display
end

;; colour patches depending on water level: use level for display and show water colours
to colour-setup
  ask patches with [ p_type = 0 ] [ set water_vol water_level - surface_level ]
  let maxWL max-water-level
  let minWL min-water-level
  ask patches with [ p_type = 0 ] [ set pcolor set-colour maxWL minWL ]
end

;; GO PROCEDURES
;; supply water to the scheme
to irrigate
  route-water
  water-losses
  color-patches
end

;; transfer water through the irrigation system (reservoir -> main canal -> secondary canals) based on irrigation scheme features (set via interface)
to route-water
  ;; assumption: totIrrAllocation of water available at source is allocated to the irrigation scheme
  set wat_allocated_irr ((totIrrAllocation / 100) * item month_number water_source)
  ask patches with [ p_type = "reservoir" ] [
    set water_vol wat_allocated_irr / count patches with [ p_type = "reservoir" ]
    set water_level water_vol + surface_level ]

  ;; wat_allocated_irr amount of water is routed to the main canal and secondary canals depending on the discharge rate, irrDaysPerDay and irrDaysPerMonth set via the interface
  ;; CHECKPOINT: Make sure we cannot discharge more water than is available for irrigation
  set hourly_water_supply dischargeRate * 60 * 60 * 0.000001 ;; in Million Cubic Metres (MCM)
  set monthly_water_supply hourly_water_supply * irrHoursPerDay * irrDaysPerMonth
  let temp dischargeRate
  while [monthly_water_supply > wat_allocated_irr] [
    set dischargeRate dischargeRate - 0.5
    set hourly_water_supply dischargeRate * 60 * 60 * 0.000001 ;; in MCM
    set monthly_water_supply hourly_water_supply * irrHoursPerDay * irrDaysPerMonth ]
  set dischargeRate temp
  ;; END OF CHECKPOINT

  ;; route water from reservoir to main canal
  ask patches with [ p_type = "mcanal" ] [
    set water_vol monthly_water_supply / count patches with [ p_type = "mcanal" ]
    set water_level water_vol + surface_level ]

  ;; route water from main canal to secondary canals
  ask patches with [ p_type = "scanal" ] [
    set water_vol monthly_water_supply / count patches with [ p_type = "scanal" ]
    set water_level surface_level + water_vol ]

  ;; route water from secondary canal to all patches in the irrigation scheme
  ;; create a patchset excluding reservoir, buffer and mcanal
  set monthly_water_use 0
  let water_recipients patches with [ p_type = 0 or p_type = "scanal" ]
  ask water_recipients [
    ;; randomly pick one neighbour with lowest water volume and move a % of the difference in volume of water (determined by benevolence parameter set via interface) to that neighbour
    let local_min 0
    let min_vol 0
    let extra 0
    let portion 0

    ;; share if patch has water
    if water_vol > 0 and monthly_water_supply > 0 and monthly_water_use < monthly_water_supply [
      set min_vol min [ water_vol ] of neighbors
      if water_vol > min_vol [
        set local_min one-of neighbors with [ water_vol = min_vol ]
        set extra water_vol - min_vol
        ifelse extra < .001
        [ set portion extra ]
        [ set portion extra * benevolence ]
        ; if portion is more than is here, just take all of it
        if portion > water_vol [ set portion water_vol ]
        ;; adjust the levels
        set water_vol water_vol - portion
        set water_level surface_level + water_vol
        ask local_min [
          set water_vol water_vol + portion
          set water_level surface_level + water_vol
        ]
      set monthly_water_use monthly_water_use + portion
      ]
    ]
  ]
end

to water-losses
  ;; reduce water level by loss-rate, which is linear and not proportional as it is due to surface area, not volume
   if loss-rate > 0 and losses?
   [ ask patches with [ water_level > surface_level ]
     [ set water_level water_level - random-float loss-rate
       if water_level < surface_level
       [ ; don't allow level to be below elev!
         set water_level surface_level
       ]
     ]
   ]
end

to color-patches
  ask patches [ set water_vol water_level - surface_level ]
  ;; set the water colour (shades of blue) of each patch based on how much water they have received compared to other water user patches
  let maxWL max-water-level
  let minWL min-water-level
  ask patches with [p_type != "buffer" ] [
    ;; CHECKPOINT to avoid math exception error for scale-color function call if all water users get the same amount of water
    ifelse (maxWL = minWL)
    [ set pcolor set-colour (mean [ water_vol ] of patches with [p_type = "mcanal" ]) minWL ]
    [ set pcolor set-colour maxWL minWL ] ]
end

;; update simulated time and time-dependent variables
to update-time
  set month_number month_number + 1                                                                ;; incrememt by one month
  if (month_number > 11) [
    clear-all-plots
    set year_number year_number + 1                                                                ;; increment year
    set month_number 0                                                                             ;; first month of the new year
    set annual_flow (list (list low_flow .1 ) (list medium_flow .7 ) (list high_flow .05 ) )       ;; choose water availability for the new year based on specified probabilities
    set water_source first rnd:weighted-one-of-list annual_flow [ [p] -> last p ]                  ;; assign water level at source for the new year based on annual_flow
  ]
end

;; helper methods ;;
;; report the closest value to input from a sourcelist
to-report closest-value [ input sourcelist ]
  report first reduce [[?1 ?2] -> ifelse-value (item 1 ?1 < item 1 ?2) [?1] [?2]] (map list sourcelist map [i -> abs(input - i)] sourcelist)
end

;; set color of the patch to shades of blue or brown depending on the amount of water (water_vol) available on patch
to-report set-colour [ maxLevel minLevel ]
  ifelse water_vol <= 0
  [ report set-earth-colour ]
  [ report set-water-colour maxLevel minLevel ]
end

;; set colour of the patch to shades of brown to signify water shortage
to-report set-earth-colour
  ifelse surface_level <= scheme_valley_middle
  [ report green - 6 + .8 * scale-color grey surface_level scheme_valley scheme_middle ]
  [ ifelse surface_level <= scheme_peak_middle
    [ report green - 4 + .8 * scale-color grey surface_level scheme_valley scheme_peak ]
    [ report green - 2 + .8 * scale-color grey surface_level scheme_middle scheme_peak ]
  ]
end

;; find max water_vol of water on any one patch across the scheme
to-report max-water-level
  let active_scheme_area patches with [p_type != "buffer" ]
  let localRecord [ water_vol ] of active_scheme_area
  set localRecord filter [ i -> i > 0 ] localRecord
  report ifelse-value (empty? localRecord) [ 0 ] [ max localRecord ]
end

;; find min water_vol of water on any one patch
to-report min-water-level
  let active_scheme_area patches with [p_type != "buffer" ]
  let localRecord [ water_vol ] of active_scheme_area
  set localRecord filter [ i -> i > 0 ] localRecord
  report ifelse-value (empty? localRecord) [ 0 ] [ min localRecord ]
end

;; set colour of patch to shades of blue based on water volume on patch
to-report set-water-colour [ maxLevel minLevel ]
  let color_gradient scale-color grey water_vol minLevel maxLevel
  ifelse color_gradient < 1 [ set color_gradient 7 - (color_gradient * 100) ] [ set color_gradient 0 ]
  ifelse color_gradient <= 0 [ set color_gradient 0 ] [ set color_gradient int color_gradient ]
  ;; greate the water_vol, darker the shade of blue on patch
  report 102 + color_gradient
end
@#$#@#$#@
GRAPHICS-WINDOW
424
10
1069
656
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-24
24
-24
24
0
0
1
ticks
30.0

BUTTON
22
41
88
74
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
94
41
157
74
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

BUTTON
163
41
226
74
tick
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
24
12
296
42
Basic Controls for Simulation
18
105.0
1

TEXTBOX
23
107
280
151
Model Settings (Irrigation)
18
105.0
1

TEXTBOX
22
140
347
166
Change values and settings below as desired
12
35.0
1

TEXTBOX
16
517
218
551
Irrigation Scheme Features
14
105.0
0

SLIDER
15
575
191
608
dischargeRate
dischargeRate
1
8
6.0
1
1
m3/s
HORIZONTAL

SLIDER
15
610
191
643
irrHoursPerDay
irrHoursPerDay
0
24
10.0
1
1
hours
HORIZONTAL

SLIDER
15
645
191
678
irrDaysPerMonth
irrDaysPerMonth
0
30
15.0
1
1
days
HORIZONTAL

TEXTBOX
21
166
331
196
Water Availability Settings
14
105.0
0

TEXTBOX
21
192
290
220
Reservoir Elevation and Volume
11
35.0
0

TEXTBOX
18
213
412
283
Data below is approximation from Tono Dam, Ghana\n[change values to suit your chosen case study] 
11
35.0
0

INPUTBOX
15
379
363
439
rElevation
[163 166 167 169 171 172 173 175 176 178 179]
1
0
String

INPUTBOX
14
445
362
505
rVolume
[0 0.239 1.42 3.5 7.9 13 22.5 37 54 77.5 92.6]
1
0
String

INPUTBOX
17
250
107
310
minRElevation
170.0
1
0
Number

INPUTBOX
110
250
202
310
meanRElevation
175.0
1
0
Number

INPUTBOX
206
250
297
310
maxRElevation
180.0
1
0
Number

INPUTBOX
16
314
109
374
surface_elevation
170.0
1
0
Number

SLIDER
15
540
191
573
totIrrAllocation
totIrrAllocation
0
100
60.0
10
1
%
HORIZONTAL

PLOT
1081
253
1412
488
water in the irrigation scheme
month
volume of water (MCM)
0.0
11.0
0.0
0.1
true
true
"" ""
PENS
"water_allocated_irr" 1.0 0 -7500403 true "" "plot monthly_water_supply"
"water_supplied" 1.0 0 -13345367 true "" "plot monthly_water_use"

PLOT
1080
10
1407
246
water available at source
month
volume (million cu. metres)
0.0
11.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "plot item month_number water_source"

SLIDER
202
540
374
573
benevolence
benevolence
0
1
0.4
0.1
1
NIL
HORIZONTAL

SWITCH
203
619
306
652
losses?
losses?
0
1
-1000

TEXTBOX
205
584
402
629
water losses from evaporation and absorption
12
105.0
1

SLIDER
202
655
342
688
loss-rate
loss-rate
0
0.005
5.0E-4
0.0005
1
NIL
HORIZONTAL

TEXTBOX
425
667
1093
877
Visualisation cues: \nPress setup: Notice the reservoir, main canal and secondary canals (parallel blue lines), and irrigable land (green patches, varying shades indicate varying heights)\n\nPress go: Notice water flowing from the reservoir -> main canal -> secondary canals -> irrigable patches. Darker the shade of blue, greater the amount of water on patch
14
25.0
1

@#$#@#$#@
## WHAT IS IT?

This NetLogo model is a Reusable Building Block (RBB) called WATERING_IRRIGATION_RBB. It is a sub-model of the WATER user associations at the Interface of Nexus Governance (WATERING) model (see https://www.youtube.com/watch?v=U-nqs9ak2nY). WATERING allows exploring the impact of community-based water governance on water availability, water use and economic productivity within an irrigation scheme. 

## HOW IT WORKS

* The plot showing water available at source (water volume) which supplies to the reservoir in the irrigation scheme follows a cosine pattern with two peaks (one at the start of a calendar year, and another in Sep-Oct each year). This assumption reflects the water availability in reservoirs we studied in Ghana (Africa). You could alter this assumption if you are interested in simulating a different pattern for water available at source in your case study. Make changes to the create-water procedure for this.

* This RBB represents an irrigation system made up of different types of patches. On setup you can identify these as: reservoir (water store providing water to the irrigation scheme), buffer zone (dummy area), main canal (principal outlet from reservoir), secondary canal (multiple outlets from the main canal), and water recipients (all green patches on setup). 

* On setup, the varying shades of green indicate the varying height of patches. Darker the green, greater the elevation of patches. Water reaches patches with the highest elevation first (assuming water is pumped up for the purpose of irrigation) and then flows to patches with lower elevations

* Water allocated for irrigation flows from the reservoir -> main canal -> secondary canals and finally reaches the water recipient patches

* The route-water process controls how water flows through the irrigation system

## HOW TO USE IT

#### Water Availability settings has 6 Input parameters:
_**minRElevation:**_ Minimum water level (elevation) in source 
_**meanRElevation:**_ Mean water level (elevation) in source
_**maxRElevation:**_ Max water level (elevation) in source
_**surface_elevation:**_ Mean surface elevation of patches in the simulated irrigation scheme
_**rElevation:**_ Shows the average water level in meters above sea level (MASL) in Tono dam (Ghana) for each month of the year
_**rVolume:**_ Shows the average volume of water in million cubic meters (MCM) in Tono dam (Ghana) corresponding to rElevation values each month. 

_**Note:**_ An Area-Volume-Elevation mapping for a reservoir plays a key role in planning reservoir operations. We use _rElevation_ and _rVolume_ as an empirical reference to create simulated low, medium and high flows (all following a cosine pattern) based on _minRElevation_, _meanRElevation_, _maxRElevation_ and _surfaceRElevation_ to act as the water source (e.g., a bigger dam) supplying to the reservoir in our simulation


#### The Irrigation Scheme Features input settings have 5 sliders:
_**totIrrAllocation:**_ Controls what % of the total water available at source is supplied to the reservoir for irrigation use
_**dischargeRate:**_ Controls the water flow rate (cubic metre per second) from the reservoir
_**irrHoursPerDay:**_ Duration of each irrigation event (0-24 hours)
_**irrDaysPerMonth:**_ Number of irrigation events each month (0 - 30 days) 
_Note:_ As the model runs in monthly time steps, the water flow you see at the end of each tick (month) is an aggregate of water supplied over the course of a month
_**benevolence:**_ A value between 0 and 1 affecting how much water each patch shares with its neighbouring patches


#### 2 more input parameters to simulate water losses from evaporation and absorption
_**losses?:**_ Do you want to include (simulate) water losses through the likes of evaporation and absorption?
_**loss_rate:**_ If losses? is true, at what rate is water lost?

## THINGS TO NOTICE

Try changing the values of sliders in the Irrigation Scheme Features and see how that affects water supply and use

## THINGS TO TRY

Try changing the values of sliders in the Irrigation Scheme Features and see how that affects water supply and use

Try changing the benevolence parameter to see how that affects water sharing between patches. Think how you might convert the benevolence parameter into a procedure that simulates different levels of willingness among water users (on water recipient patches) to share water with other water users

## EXTENDING THE MODEL

What we are trying to achieve here is to simulate the routing of water available at source through a canal network in an irrigation scheme by mimicking gravity-based flow, whereby water flows from a higher elevation to lower elevations

The plot called _water in the irrigation scheme_ shows how much of the water allocated for irrigation is being effectively supplied to water recipients. Try making changes to the route-water procedure to see if you can minimise the difference between water supply and water use


## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

The WATERING_irrigation RBB is an adaptation of the Watershed model (http://ccl.northwestern.edu/netlogo/models/community/watershed)

## CREDITS AND REFERENCES

Please email Dr Kavin Narasimhan (k.narasimhan@surrey.ac.uk) for comments/queries. If you adapt/use the WATERING_IRRIGATION_RBB model, we would appreciate if you cite our GitHub repo, as well as the Watershed model (http://ccl.northwestern.edu/netlogo/models/community/watershed) based on which we have created this model. Acknowledgment: This work was supported by UK Research and Innovation Economic and Social Research Council [ES/P011373/1] as part of the Global Challenges Research Fund.
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
NetLogo 6.1.1
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
