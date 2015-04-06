
## FROM:  http://www.state.nj.us/transportation/refdata/accident/policecrashtypedef.shtm
##        Accessed 2015-03-21 08:43:59 EDT

## ------------------------------------------------ ##
## Crash Records
## Police Resources
## Crash Type Definitions
##
## Definitions of Identifiable Crash Types Proposed for Use as Part of Updated NJTR-1 Police Crash Investigation Report Form
##
## ------------------------------------------------ ##

dict_DT.Crash_Types <- 
as.data.table(read.table(header=TRUE, sep="|", colClasses=c("character"), text=
"code|title|description
01|Same Direction (Rear-end)|Two vehicles in a position of one behind the other and collide, regardless of what movement(s) either vehicle was in the process of making with the exception of one or both vehicles backing. This type includes a collision in which the leading vehicle spun out and became turned 180 degrees around such that the resulting same direction collision had it strike front end to front end with the following vehicle.
02|Same Direction (Sideswipe)|Two vehicles moving alongside each other and collide, with at least one of the vehicles being struck on the side. This type would include a collision resulting from one of the vehicles making an improper turn such as a left from the right lane or vice-versa or turning right from the appropriate outside lane and striking a vehicle passing on the right shoulder.
03|Right Angle|Two vehicles approaching from non-opposing angular directions collide, typically resulting as one vehicle failed to either stop or yield right of way from a Stop or Yield sign, ran a red light, or was not cleared from the intersection upon the onset of the conflicting movement@s green signal.
04|Opposite Direction (Head-on/Angular)|Two vehicles approaching opposite directions and intending to continue in opposite directions collide in a frontal or angular manner as a result of one or both vehicles crossing the painted or unpainted centerline or divided median of the roadway. This includes a collision resulting from one vehicle traveling the wrong way down a divided highway.
05|Opposite Direction (Sideswipe)|Two vehicles approaching opposite directions and intending to continue in opposite directions collide in a sideswiping manner as a result of one or both vehicles crossing the painted or unpainted centerline or divided median of the roadway. This also includes a collision resulting from one vehicle traveling the wrong way down a divided highway.
06|Parked Vehicle|A crash involving a vehicle in transport striking a parked vehicle within the roadway or in a parking lot.
07|Left Turn/U Turn|Two vehicles approaching from opposite directions collide as a result of at least one vehicle attempting to make a left or U turn in front of the opposing vehicle.
08|Backing|This type of crash, previously labeled as \"Other\" type, is defined as any multi-vehicle collision when at least one vehicle was in the act of backing.
09|Encroachment|Previously labeled as \"Other\" type crash, but frequently mislabeled as an angle crash due to the approach directions of one of the turning vehicles and a stopped, starting or slowing vehicle on an adjacent approach, this crash defines the collision of two adjacent approach vehicles whose paths are unintended to come in conflict, but collide as a result of one or both vehicles over- or under-turning. This type would include a vehicle initially going straight, but leaving its proper lane of travel and colliding with a stopped or moving vehicle on an adjacent approach road or driveway.
10|Overturned|A crash in which a vehicle overturns on or off the roadway without first having been involved in some other type single or multiple vehicle crash. This includes motorcycle crashes in which the operator loses control of and drops bike, but had not initially struck another motor vehicle, fixed or non-fixed object, animal, pedacyclist or pedestrian.
11|Fixed Object|A crash in which the primary collision involved a single vehicle and a fixed object.
12|Animal|A crash involving a vehicle striking any animal, including a deer. However, a deer crash could also be so-named for specific identification of this more common type animal crash within the appropriate box on the Police Crash Report form.
13|Pedestrian|A crash involving a vehicle and pedestrian in which the collision between the two is the first event and also took place within the road proper. This type includes a vehicle colliding with someone walking their bicycle in the roadway.
14|Pedacycle|A crash involving a vehicle and a bicycle that is in the act of being ridden or stopped in the roadway, but currently mounted by the cyclist.
15|Non-fixed object|Excluding the single motor vehicle type crashes defined in numb ers 10-14 above, this type implies any crash initially involving a single vehicle and object not considered a fixed or permanent condition of the highway like ruts, bumps, sink- or potholes or other miscellaneous stationary or airborne road debris such as garbage, tree limbs, fallen-off parts of other vehicles, broken and scattered signs/posts, etc.
16|Railcar-vehicle|Any crash involving a vehicle and a train, trolley, light transit or other type railcar that occurred within a roadway right-of-way or at an at-grade intersection.
99|Other|This category encompasses all other categories of single and multi-vehicle crashes that are not defined above. These include, but are not limited to, all other non-collision events such as immersion, cargo loss, separation of units, fire/explosion, and run-off road incidents (whereby damage is caused to the vehicle, but nothing else was physically struck during or following the act of leaving the highway).
00|Unknown|Unknown"
))

## The single quote was messing up read.table - not sure why
dict_DT.Crash_Types[, description := gsub("movement@s", "movement's", description)]

setkey(dict_DT.Crash_Types, code)


# Crash_Types[1, list(list(list(V3)))]
# Crash_Types[, V4 := list(list(list(V3))), by=V1]
# Crash_Types[, V3 := NULL]

