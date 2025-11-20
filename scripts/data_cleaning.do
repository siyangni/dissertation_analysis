***Data Cleaning***

clear all
set more off // do not truncate output
set maxvar 30000 // Set max variable number to 30000

// Go to the working directory
cd "C:\Users\siyan\OneDrive - The Pennsylvania State University\dissertation_working\data\"

* Load original merged dataset
use mergedwaves.dta

* SECTION 1: SELF-CONTROL RECODE - AGE 3

* Age 3 - Positive items (1→0, 2→1, 3→2)

* Think before acting
recode bmsdsta0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-2 -1 4 = .), gen(sc3thac)
label variable sc3thac "Think before act"

* See tasks through
recode bmsdtea0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-2 -1 4 = .), gen(sc3tcom)
label variable sc3tcom "See tasks through" 

* Obedient
recode bmsdora0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-2 -1 4 = .), gen(sc3obey)
label variable sc3obey "Obedient" 

* Age 3 - Negative items (1→2, 2→1, 3→0) - REVERSE CODED

* Easily distracted
recode bmsddca0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-2 -1 4 = .), gen(sc3dist)
label variable sc3dist "Easily distracted" 

* Temper tantrums
recode bmsdtta0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-2 -1 4 = .), gen(sc3temp)
label variable sc3temp "Temper tantrums"

* Restless
recode bmsdroa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-2 -1 4 = .), gen(sc3rest)
label variable sc3rest "Restless"

* Constantly fidgeting
recode bmsdfsa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-2 -1 4 = .), gen(sc3fidg)
label variable sc3fidg "Constantly fidgeting"

///
* Age 5 - Positive items (1→0, 2→1, 3→2)

* Think before acting
recode cmsdsta0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-9 -8 -1 4 = .), gen(sc5thac)
label variable sc5thac "Think before act"

* See tasks through
recode cmsdtea0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-9 -8 -1 4 = .), gen(sc5tcom)
label variable sc5tcom "See tasks through" 

* Obedient
recode cmsdora0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-9 -8 -1 4 = .), gen(sc5obey)
label variable sc5obey "Obedient" 

* Age 5 - Negative items (1→2, 2→1, 3→0) - REVERSE CODED

* Easily distracted
recode cmsddca0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc5dist)
label variable sc5dist "Easily distracted" 

* Temper tantrums
recode cmsdtta0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc5temp)
label variable sc5temp "Temper tantrums"

* Restless
recode cmsdroa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc5rest)
label variable sc5rest "Restless"

* Constantly fidgeting
recode cmsdfsa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc5fidg)
label variable sc5fidg "Constantly fidgeting"

* Often lying or cheating
recode cmsdoaa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc5lyin)
label variable sc5lyin "Often lying or cheating"

///
* Age 7 - Positive items (1→0, 2→1, 3→2)

* Think before acting
recode dmsdsta0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-9 -8 -1 4 = .), gen(sc7thac)
label variable sc7thac "Think before act"

* See tasks through
recode dmsdtea0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-9 -8 -1 4 = .), gen(sc7tcom)
label variable sc7tcom "See tasks through" 

* Obedient
recode dmsdora0 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-9 -8 -1 4 = .), gen(sc7obey)
label variable sc7obey "Obedient" 

* Age 7 - Negative items (1→2, 2→1, 3→0) - REVERSE CODED

* Easily distracted
recode dmsddca0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc7dist)
label variable sc7dist "Easily distracted" 

* Temper tantrums
recode dmsdtta0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc7temp)
label variable sc7temp "Temper tantrums"

* Restless
recode dmsdroa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc7rest)
label variable sc7rest "Restless"

* Constantly fidgeting
recode dmsdfsa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc7fidg)
label variable sc7fidg "Constantly fidgeting"

* Often lying or cheating
recode dmsdoaa0 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-9 -8 -1 4 = .), gen(sc7lyin)
label variable sc7lyin "Often lying or cheating"


********************************************************************************
* SECTION 1D: SELF-CONTROL RECODE - AGE 11
* Age 11 - Positive items (1→0, 2→1, 3→2)

* Think before acting
recode epsdst00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 4 = .), gen(sc11thac)
label variable sc11thac "Think before act"

* See tasks through
recode epsdte00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 4 = .), gen(sc11tcom)
label variable sc11tcom "See tasks through" 

* Obedient
recode epsdor00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 4 = .), gen(sc11obey)
label variable sc11obey "Obedient" 

* Age 11 - Negative items (1→2, 2→1, 3→0) - REVERSE CODED

* Easily distracted
recode epsddc00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc11dist)
label variable sc11dist "Easily distracted" 

* Temper tantrums
recode epsdtt00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc11temp)
label variable sc11temp "Temper tantrums"

* Restless
recode epsdro00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc11rest)
label variable sc11rest "Restless"

* Constantly fidgeting
recode epsdfs00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc11fidg)
label variable sc11fidg "Constantly fidgeting"

* Often lying or cheating
recode epsdoa00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc11lyin)
label variable sc11lyin "Often lying or cheating"

//
* Age 14 - Positive items

* Think before acting
recode fpsdst00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 -9 = .), gen(sc14thac)
label variable sc14thac "Think before act"

* See tasks through
recode fpsdte00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 -9 = .), gen(sc14tcom)
label variable sc14tcom "See tasks through" 

* Obedient
recode fpsdor00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 -9 = .), gen(sc14obey)
label variable sc14obey "Obedient" 

* Age 14 - Negative items (1→2, 2→1, 3→0) - REVERSE CODED

* Easily distracted
recode fpsddc00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 -9 = .), gen(sc14dist)
label variable sc14dist "Easily distracted" 

* Temper tantrums
recode fpsdtt00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 -9 = .), gen(sc14temp)
label variable sc14temp "Temper tantrums"

* Restless
recode fpsdro00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 -9 = .), gen(sc14rest)
label variable sc14rest "Restless"

* Constantly fidgeting
recode fpsdfs00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 -9 = .), gen(sc14fidg)
label variable sc14fidg "Constantly fidgeting"

* Often lying or cheating
recode fpsdoa00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 -9 = .), gen(sc14lyin)
label variable sc14lyin "Often lying or cheating"

//
* SECTION 1F: SELF-CONTROL RECODE - AGE 17

* Age 17 - Positive items (1→0, 2→1, 3→2)

* Think before acting
recode gpsdst00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 4 = .), gen(sc17thac)
label variable sc17thac "Think before act"

* See tasks through
recode gpsdte00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 4 = .), gen(sc17tcom)
label variable sc17tcom "See tasks through" 

* Obedient
recode gpsdor00 (1=0 "Not true") (2=1 "Somewhat true") (3=2 "Certainly true") (-1 4 = .), gen(sc17obey)
label variable sc17obey "Obedient" 

* Age 17 - Negative items (1→2, 2→1, 3→0) - REVERSE CODED

* Easily distracted
recode gpsddc00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc17dist)
label variable sc17dist "Easily distracted" 

* Temper tantrums
recode gpsdtt00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc17temp)
label variable sc17temp "Temper tantrums"

* Restless
recode gpsdro00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc17rest)
label variable sc17rest "Restless"

* Constantly fidgeting
recode gpsdfs00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc17fidg)
label variable sc17fidg "Constantly fidgeting"

* Often lying or cheating
recode gpsdoa00 (1=2 "Not true") (2=1 "Somewhat true") (3=0 "Certainly true") (-1 4 = .), gen(sc17lyin)
label variable sc17lyin "Often lying or cheating"



***SECTION 2: PARENTING VARIABLES RECODE***

** Age 3

* How often ignores CM when naughty
recode bmdiig00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(ignore3)
label variable ignore3 "How often ignores CM when naughty (recoded)"

* How often smacks CM when naughty
recode bmdism00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(smack3)
label variable smack3 "How often smacks CM when naughty (recoded)"

* How often shouts at CM when naughty
recode bmdish00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(shout3)
label variable shout3 "How often shouts at CM when naughty (recoded)"

* How often sends CM to bedroom/naughty chair
recode bmdibn00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(bedroom3)
label variable bedroom3 "How often sends CM to bedroom/naughty chair (recoded)"

* How often takes away treats from CM when naughty
recode bmditr00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(treats3)
label variable treats3 "How often takes away treats from CM when naughty (recoded)"

* How often tells CM off when naughty
recode bmdite00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(telloff3)
label variable telloff3 "How often tells CM off when naughty (recoded)"

* How often bribes CM when naughty
recode bmdibr00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(bribe3)
label variable bribe3 "How often bribes CM when naughty (recoded)"

** Age 5

* How often ignores CM when naughty
recode cmdiiga0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(ignore5)
label variable ignore5 "How often ignores CM when naughty (recoded)"

* How often smacks CM when naughty
recode cmdisma0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(smack5)
label variable smack5 "How often smacks CM when naughty (recoded)"

* How often shouts at CM when naughty
recode cmdisha0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(shout5)
label variable shout5 "How often shouts at CM when naughty (recoded)"

* How often sends CM to bedroom/naughty chair
recode cmdibna0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(bedroom5)
label variable bedroom5 "How often sends CM to bedroom/naughty chair (recoded)"

* How often takes away treats from CM when naughty
recode cmditra0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(treats5)
label variable treats5 "How often takes away treats from CM when naughty (recoded)"

* How often tells CM off when naughty
recode cmditea0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(telloff5)
label variable telloff5 "How often tells CM off when naughty (recoded)"

* How often bribes CM when naughty
recode cmdibra0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(bribe5)
label variable bribe5 "How often bribes CM when naughty (recoded)"

* How often tries to reason with CM when naughty
recode cmdirea0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(reason5)
label variable reason5 "How often tries to reason with CM when naughty (recoded)"

* How often family does indoor activities together (reverse coded)
recode cmfrtv00 (7=0 "Less often or never") (6=1 "At least once a year") (5=2 "Every few months") (4=3 "At least once a month") (3=4 "Once or twice a week") (2=5 "Several times a week") (1=6 "Every day or almost every day") (-9 -8 -1 = .), gen(rindoor5)
label variable rindoor5 "How often family does indoor activities together (reverse coded)"

* Who eats weekday evening meal with CM (binary: parents vs not)
recode cmevwoaa (2=1 "Parent(s)/Guardian(s)") (1 3 4 5 6 95=0 "Not Parents") (-9 -8 -1 = .), gen(dinner5)
label variable dinner5 "Eats weekday evening meal with parent(s)/guardian(s)"

* Closeness to CM
recode cmschca (1=0 "Not very close") (2=1 "Fairly close") (3=2 "Very close") (4=3 "Extremely close") (-9 -8 -1 5 = .), gen(close5)
label variable close5 "Closeness to CM (recoded)"

* How often reads to CM (reverse coded)
recode cmreofa0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(read5)
label variable read5 "How often reads to CM (recoded)"

* How often tells stories to CM (reverse coded)
recode cmsitsa0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(story5)
label variable story5 "How often tells stories to CM (recoded)"

* How often does musical activities with CM (reverse coded)
recode cmplmua0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(music5)
label variable music5 "How often does musical activities with CM (recoded)"

* How often draws/paints with CM (reverse coded)
recode cmpamaa0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(paint5)
label variable paint5 "How often draws/paints with CM (recoded)"

* How often plays physically active games with CM (reverse coded)
recode cmactia0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(active5)
label variable active5 "How often plays physically active games with CM (recoded)"

* How often plays games/toys indoors with CM (reverse coded)
recode cmgamea0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(games5)
label variable games5 "How often plays games/toys indoors with CM (recoded)"

* How often takes CM to park/playground (reverse coded)
recode cmwalka0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(park5)
label variable park5 "How often takes CM to park/playground (recoded)"

//
** Age 7 

* How often ignores CM when naughty
recode dmdiiga0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(ignore7)
label variable ignore7 "How often ignores CM when naughty (recoded)"

* How often smacks CM when naughty
recode dmdisma0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(smack7)
label variable smack7 "How often smacks CM when naughty (recoded)"

* How often shouts at CM when naughty
recode dmdisha0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(shout7)
label variable shout7 "How often shouts at CM when naughty (recoded)"

* How often sends CM to bedroom/naughty chair
recode dmdibna0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(bedroom7)
label variable bedroom7 "How often sends CM to bedroom/naughty chair (recoded)"

* How often takes away treats from CM when naughty
recode dmditra0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(treats7)
label variable treats7 "How often takes away treats from CM when naughty (recoded)"

* How often tells CM off when naughty
recode dmditea0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(telloff7)
label variable telloff7 "How often tells CM off when naughty (recoded)"

* How often bribes CM when naughty
recode dmdibra0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(bribe7)
label variable bribe7 "How often bribes CM when naughty (recoded)"

* How often tries to reason with CM when naughty
recode dmdirea0 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-9 -8 -1 6 = .), gen(reason7)
label variable reason7 "How often tries to reason with CM when naughty (recoded)"

* How often family does indoor activities together (reverse coded)
recode dmfrtv00 (6=0 "Not at all") (5=1 "Less often than once a month") (4=2 "Once or twice a month ") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day or almost every day") (-9 -8 -1 = .), gen(rindoor7)
label variable rindoor7 "How often family does indoor activities together (reverse coded)"

* Who eats weekday evening meal with CM (binary: parents vs not)
recode dmevwoaa (2=1 "Parent(s)/Guardian(s)") (1 3 4 5 6 7 8 95=0 "Not Parents") (-9 -8 -1 = .), gen(dinner7)
label variable dinner7 "Eats weekday evening meal with parent(s)/guardian(s)"

* Closeness to CM
recode dmschca (1=0 "Not very close") (2=1 "Fairly close") (3=2 "Very close") (4=3 "Extremely close") (-9 -8 -1 5 = .), gen(close7)
label variable close7 "Closeness to CM (recoded)"

* How often reads to CM (reverse coded)
recode dmreofa0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(read7)
label variable read7 "How often reads to CM (recoded)"

* How often tells stories to CM (reverse coded)
recode dmsitsa0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(story7)
label variable story7 "How often tells stories to CM (recoded)"

* How often does musical activities with CM (reverse coded)
recode dmplmua0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(music7)
label variable music7 "How often does musical activities with CM (recoded)"

* How often draws/paints with CM (reverse coded)
recode dmpamaa0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(paint7)
label variable paint7 "How often draws/paints with CM (recoded)"

* How often plays physically active games with CM (reverse coded)
recode dmactia0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(active7)
label variable active7 "How often plays physically active games with CM (recoded)"

* How often plays games/toys indoors with CM (reverse coded)
recode dmgamea0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(games7)
label variable games7 "How often plays games/toys indoors with CM (recoded)"

* How often takes CM to park/playground (reverse coded)
recode dmwalka0 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-9 -8 -1 = .), gen(park7)
label variable park7 "How often takes CM to park/playground (recoded)"

* TV rules - limited hours
recode dmtvrha0 (1=1 "Yes") (2=0 "No") (-9 -8 -1 = .), gen(tvrules7)
label variable tvrules7 "TV rules - limited hours"

//
** Age 11

* How often sends CM to bedroom/naughty chair
recode epdibn00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(bedroom11)
label variable bedroom11 "How often sends CM to bedroom/naughty chair (recoded)"

* How often takes away treats from CM when naughty
recode epditr00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(treats11)
label variable treats11 "How often takes away treats from CM when naughty (recoded)"

* How often tries to reason with CM when naughty
recode epdire00 (1=0 "Never") (2=1 "Rarely") (3=2 "Sometimes (about once a month)") (4=3 "Often (about once a week or more)") (5=4 "Daily") (-1 6 = .), gen(reason11)
label variable reason11 "How often tries to reason with CM when naughty (recoded)"

* Closeness to CM
recode epschc00 (1=0 "Not very close") (2=1 "Fairly close") (3=2 "Very close") (4=3 "Extremely close") (-1 5 = .), gen(close11)
label variable close11 "Closeness to CM (recoded)"

* How often plays physically active games with CM (reverse coded)
recode epacti00 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-8 -1 = .), gen(active11)
label variable active11 "How often plays physically active games with CM (recoded)"

* How often plays games/toys indoors with CM (reverse coded)
recode epgame00 (6=0 "Not at all") (5=1 "Less often") (4=2 "Once or twice a month") (3=3 "Once or twice a week") (2=4 "Several times a week") (1=5 "Every day") (-1 = .), gen(games11)
label variable games11 "How often plays games/toys indoors with CM (recoded)"

//
* Parental monitoring - Age 14
// fp* are in the Parent questionnaire, fc* are in the young person questionaire

* Where child is (reverse coded)
recode fpwhet00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (-8 -1 = .), gen(pwhere14)
label variable pwhere14 "Knows where child is (age 14)"

* Who child is with (reverse coded)
recode fpwhot00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (-8 -1 = .), gen(pwho14)
label variable pwho14 "Knows who child is with (age 14)"

* What child is doing (reverse coded)
recode fpwhat0a (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (-8 -1 = .), gen(pwhat14)
label variable pwhat14 "Knows what child is doing (age 14)"

* When CM goes out, how often do parents know where?  (reverse coded)
recode fcoutw00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (-9 -8 -1 = .), gen(cmwhere14)
label variable cmwhere14 "When CM goes out, how often do parents know where? (age 14)"

* When CM goes out, how often do parents know who with? (reverse coded)
recode fcotwi00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (-9 -8 -1 = .), gen(cmwho14)
label variable cmwho14 "When CM goes out, how often do parents know who with? (age 14)"

* When CM goes out, how often do parents know what CM does? (reverse coded)
recode fcotwd00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (-9 -8 -1 = .), gen(cmwhat14)
label variable cmwhat14 "When CM goes out, how often do parents know what CM does? (age 14)"

//
* Parental monitoring - Age 17
// gp* are in the Parent questionnaire, gc* are in the young person questionaire

* How often do your parents know where you are going when you go out? (reverse coded)
recode gcoutw00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (5 7 = .), gen(cmwhere17)
label variable cmwhere17 " How often do your parents know where you are going when you go out? (Age 17)"

* Set time to be back at night - cohort member report (reverse coded)
recode gcotnt00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (-8 -1 5 = .), gen(cmtback17)
label variable cmtback17 "Sets time to be back at night (age 17)"

* Knows where cohort member is going - parent report (reverse coded)
recode gpwhet00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (5 6 7 = .), gen(pwhere17)
label variable pwhere17 "Knows where CM is going - parent report (age 17)"

* Sets time for cohort member to be back - parent report (reverse coded)
recode gpwhut00 (4=0 "Never") (3=1 "Sometimes") (2=2 "Usually") (1=3 "Always") (5 6 7 = .), gen(ptback17)
label variable ptback17 "Sets time to be back - parent report (age 17)"

* Do you live with your parent(s)?
recode gclwpa00 (2=0 "No") (1=1 "Yes") (3 4 5 = .), gen(livewp17)
label variable livewp17 "Do you live with your parent(s)? (age 17)"


//
* SECTION 3: CONTROL VARIABLES RECODE

* Parents education (subtract 1 from original scale)
recode amacqu00 (1=0) (2=1) (3=2) (4=3) (5=4) (6=5) (-1 -8 -9 95 96 = .), gen(pedu)
label variable pedu "Parents education level"

* Parents education binary (0=Higher education, 1=Lower education)
recode pedu (0 1=1 "No higher education") (2 3 4 5=0 "Higher education"), gen(bpedu)
label variable bpedu "Parents education binary"

* Child sex (0=Male, 1=Female)
recode ahcsexa0 (1=0 "Male") (2=1 "Female"), gen(sex)
label variable sex "Child sex"

* Race/ethnicity (4 categories)
recode adceeaa0 (1 2 3=1 "White") (8 9 10 11 15=2 "Asian") (12 13 14=3 "Black") (4 5 6 7 95=4 "Mixed/Other") (-9 -8 -1 = .), gen(race)
label variable race "Race/ethnicity"

* Race binary (0=White, 1=Non-White)
recode race (1=0 "White") (2 3 4=1 "Non-White"), gen(brace)
label variable brace "Race binary"

* Marital status (0=Not married, 1=Married)
recode amfcin00 (1 4 5 6=0 "Not married/cohabiting") (2 3=1 "Married/cohabiting") (9 8 -1 = .), gen(bmarried)
label variable bmarried "Marital status"

* Parents income - couple
gen incomec = amnico00
replace incomec = . if inlist(amnico00, -1, 96, 97)
label variable incomec "Parents income - couple"

* Parents income - lone parent
gen incomel = amnilp00
replace incomel = . if inlist(amnilp00, -1, 96, 97)
label variable incomel "Parents income - lone parent"

* Family income (banded)
recode adhinc00 (1=0 "£0 to less than £3100 pa") (2=1 "£3100 to less than £10400 pa") (3=2 "£10400 to less than £20800 pa") (4=3 "£20800 to less than £31200 pa") (5=4 "£31200 to less than £52000 pa") (6=5 "£52000 and above pa") (-6 -1 96 97 = .), gen(incomef)
label variable incomef "Family income (banded, recoded)"

* Low birthweight (1=Yes if <2.5kg, 0=No if >=2.5kg)
gen lbw = .
replace lbw = 1 if adbwgta0 < 2.5 & adbwgta0 > 0
replace lbw = 0 if adbwgta0 >= 2.5 & adbwgta0 != .
label variable lbw "Low birthweight indicator"
label define lbw_lbl 0 "Normal/high birthweight" 1 "Low birthweight"
label values lbw lbw_lbl

* Infant temperament items
//
* Carey Infant Temperament Scale - Mood Subscale (5 items)

* HAPN: Makes happy sounds when having nappy changed or being dressed (Mood)
recode amhapna0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(namhapna0)
label variable namhapna0 "Makes happy sounds during nappy change/dressing"

* UNFA: Pleasant when arriving in unfamiliar places (Mood)
recode amunfaa0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(namunfaa0)
label variable namunfaa0 "Pleasant when arriving in unfamiliar places"

* BRUS: Pleasant during procedures like hair brushing/face washing (Mood)
recode ambrusa0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(nambrusa0)
label variable nambrusa0 "Pleasant during hair brushing/face washing"

* FEED: Content during interruptions of feeding (Mood)
recode amfeeda0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(namfeeda0)
label variable namfeeda0 "Content during interruptions of feeding"

* INJU: Remains pleasant/calm with minor injuries (Mood)
recode aminjua0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(naminjua0)
label variable naminjua0 "Remains pleasant/calm with minor injuries"

* ==============================================================================
* Carey Infant Temperament Scale - Approach/Withdrawal Subscale (3 items)
* All items reverse coded
* ==============================================================================

* BATH: Objects to bathing in different place/by different person (Approach/Withdrawal - REVERSE)
recode ambatha0 (5=1 "Almost never") (4=2 "Rarely") (3=3 "Usually does not") ///
    (2=4 "Often") (1=5 "Almost always") (-9 -8 -1 6 = .), gen(nambatha0)
label variable nambatha0 "Objects to bathing in different place/by different person (R)"

* WARY: Still wary/frightened of strangers after 15 minutes (Approach/Withdrawal - REVERSE)
recode amwarya0 (5=1 "Almost never") (4=2 "Rarely") (3=3 "Usually does not") ///
    (2=4 "Often") (1=5 "Almost always") (-9 -8 -1 6 = .), gen(namwarya0)
label variable namwarya0 "Still wary/frightened of strangers after 15 minutes (R)"

* BSHY: Shy when meeting another child for first time (Approach/Withdrawal - REVERSE)
recode ambshya0 (5=1 "Almost never") (4=2 "Rarely") (3=3 "Usually does not") ///
    (2=4 "Often") (1=5 "Almost always") (-9 -8 -1 6 = .), gen(nambshya0)
label variable nambshya0 "Shy when meeting another child for first time (R)"

* ==============================================================================
* Carey Infant Temperament Scale - Adaptability Subscale (2 items)
* All items reverse coded
* ==============================================================================

* FRET: Fretful for first few minutes in new place/situation (Adaptability - REVERSE)
recode amfreta0 (5=1 "Almost never") (4=2 "Rarely") (3=3 "Usually does not") ///
    (2=4 "Often") (1=5 "Almost always") (-9 -8 -1 6 = .), gen(namfreta0)
label variable namfreta0 "Fretful for first few minutes in new place/situation (R)"

* SLEE: Bothered when first put down in different sleeping place (Adaptability - REVERSE)
recode amsleea0 (5=1 "Almost never") (4=2 "Rarely") (3=3 "Usually does not") ///
    (2=4 "Often") (1=5 "Almost always") (-9 -8 -1 6 = .), gen(namsleea0)
label variable namsleea0 "Bothered when first put down in different sleeping place (R)"

* ==============================================================================
* Carey Infant Temperament Scale - Regularity Subscale (4 items)
* ==============================================================================

* MILK: Wants/takes milk feeds at about same time daily (Regularity)
recode ammilka0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(nammilka0)
label variable nammilka0 "Wants/takes milk feeds at about same time daily"

* SLTI: Gets sleepy at about same time each evening (Regularity)
recode amsltia0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(namsltia0)
label variable namsltia0 "Gets sleepy at about same time each evening"

* NAPS: Naps are about same length from day to day (Regularity)
recode amnapsa0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(namnapsa0)
label variable namnapsa0 "Naps are about same length from day to day"

* SOFO: Wants/takes solid food at about same time daily (Regularity)
recode amsofoa0 (1=1 "Almost never") (2=2 "Rarely") (3=3 "Usually does not") ///
    (4=4 "Often") (5=5 "Almost always") (-9 -8 -1 6 = .), gen(namsofoa0)
label variable namsofoa0 "Wants/takes solid food at about same time daily"

* Create raw infant temperament score (sum of all items)
egen inftempr = rowtotal(namhapna0 namunfaa0 nambrusa0 ///
    namfeeda0 naminjua0 nambatha0 namwarya0 nambshya0 namfreta0 ///
    namsleea0 nammilka0 namsltia0 namnapsa0 namsofoa0), missing
label variable inftempr "Infant temperament raw score"

* Standardize to create z-score
egen inftemp = std(inftempr)
label variable inftemp "Infant temperament (standardized)"

//
* Fetal alcohol exposure variables
// 0 for non drinkers, 1 for consuming ≥5 units per day or consuming ≥7 units per week
* Drinking frequency per week
* Drinking frequency per week
gen dfpw = .
replace dfpw = 7 if amdrof00 == 1
replace dfpw = 5.5 if amdrof00 == 2
replace dfpw = 3.5 if amdrof00 == 3
replace dfpw = 1.5 if amdrof00 == 4
replace dfpw = 0.375 if amdrof00 == 5
replace dfpw = 0.25 if amdrof00 == 6
replace dfpw = 0 if amdrof00 == 7
replace dfpw = . if amdrof00 < 0

label variable dfpw "Drinking frequency per week"

* Units per day 
gen dupd = ampuda00
replace dupd = . if ampuda00 < 0 | ampuda00 > 22
label variable dupd "Units per day"

* Units per week
gen dupw = ampuwk00
replace dupw = 0 if ampuwk00 < 0 | ampuwk00 > 100
label variable dupw "Units per week"

* Heavy fetal alcohol exposure (binary indicator)
gen hfae = .
replace hfae = 0 if dfpw == 0
replace hfae = 1 if dupd >= 5 & ///
    dupd != .
replace hfae = 1 if dupw >= 7 & ///
    dupw != .
replace hfae = 0 if dfpw != . & ///
    dupd != . & hfae == .
label variable hfae "Heavy fetal alcohol exposure"

* Cognitive ability at age 3

* Raw cognitive ability score
gen coga = bdsrcs00
replace coga = . if bdsrcs00 < 0
label variable coga "Cognitive ability raw score (age 3)"

* Standardize to create z-score
egen scoga = std(coga)
label variable scoga "Cognitive ability (standardized, age 3)"

// Age
* 1. Create the new variable by flooring only non-negative values
gen age3 = floor(bhcagea0/365.25) if bhcagea0>=0
label var age3 "Age (years, completed) at MCS2 interview (from days)" 

gen age5 = floor(chcagea0/365.25) if chcagea0>=0
label var age5 "Age (years, completed) at MCS3 interview (from days)" 

gen age7 = floor(dmcs4age) if dmcs4age >= 0
label var age7 "Age (years, completed) at MCS4 interview (from days)" 

gen age11 = floor(emcs5age) if emcs5age >=0 
label var age11 "Age (years, completed) at MCS5 interview (from days)" 

gen age14 = floor(fcmcs6ag) if fcmcs6ag >=0 
label var age14 "Age (years, completed) at MCS6 interview (from days)" 

gen age17 = floor(gcmcs7ag) if gcmcs7ag >=0 
label var age17 "Age (years, completed) at MCS7 interview (from days)" 

* Check ranges by sweep (completed years)
summ age3 age5 age7 age11 age14 age17

//
* Save recoded dataset
save "merged_waves_recoded"

//
* Keep only recoded variables
* Keep only variables created/re-coded above
keep ///
	mcsid pttype2 sptn00 nh2 bovwt1 covwt1 dovwt1 eovwt1 fovwt1 govwt1 ///
    sc3thac sc3tcom sc3obey sc3dist sc3temp sc3rest sc3fidg ///
    sc5thac sc5tcom sc5obey sc5dist sc5temp sc5rest sc5fidg sc5lyin ///
    sc7thac sc7tcom sc7obey sc7dist sc7temp sc7rest sc7fidg sc7lyin ///
    sc11thac sc11tcom sc11obey sc11dist sc11temp sc11rest sc11fidg sc11lyin ///
    sc14thac sc14tcom sc14obey sc14dist sc14temp sc14rest sc14fidg sc14lyin ///
    sc17thac sc17tcom sc17obey sc17dist sc17temp sc17rest sc17fidg sc17lyin ///
	ignore3 smack3 shout3 bedroom3 treats3 telloff3 bribe3 ///
    ignore5 smack5 shout5 bedroom5 treats5 telloff5 bribe5 reason5 ///
    rindoor5 dinner5 close5 read5 story5 music5 paint5 active5 games5 park5 ///
    ignore7 smack7 shout7 bedroom7 treats7 telloff7 bribe7 reason7 ///
    rindoor7 dinner7 close7 read7 story7 music7 paint7 active7 games7 park7 tvrules7 ///
    bedroom11 treats11 reason11 close11 active11 games11 ///
    pwhere14 pwho14 pwhat14 cmwhere14 cmwho14 cmwhat14 ///
    cmwhere17 cmtback17 pwhere17 ptback17 livewp17 ///
    pedu bpedu sex race brace bmarried incomec incomel incomef lbw ///
    namhapna0 namunfaa0 nambrusa0 namfeeda0 naminjua0 ///
    nambatha0 namwarya0 nambshya0 ///
    namfreta0 namsleea0 ///
    nammilka0 namsltia0 namnapsa0 namsofoa0 ///
    inftempr inftemp ///
    dfpw dupd dupw hfae ///
    coga scoga ///
    age3 age5 age7 age11 age14 age17

//
* Save the subset
save "recoded_only_sc_pa_cov"
* Export to mplus
stata2mplus using "recoded_only_sc_pa_cov"