(* Get data and setup *)

<< MyFunctionLibrary`

site = Import["https://giveawaylisting.com/", "XMLObject"];

scrapeGiveaway[page_] := Cases[page,
     
     XMLElement["tr", _,
       {XMLElement[
         "td", _, {XMLElement["a", {_, "href" -> url_, _}, {time_}]}],
         XMLElement["td", _, {odds_}], XMLElement["td", _, {max_}], 
        XMLElement["td", _, {OGPrice_}],
        _,
        XMLElement["td", _, {XMLElement["a", _, {item_}]}],
        XMLElement["td", _, {seller_}]}]
      :>
      {
       time,
       max,
       odds,
       OGPrice,
       item,
        seller,
       url
       }
     , Infinity
     ] //. XMLElement[__, x_] :> x /. {x_String} :> x;

table = scrapeGiveaway[site];

data = monitorMap[
   
   {
     parseTime[#[[1]]],
     ToExpression@#[[2]],
     ToExpression@#[[3]],
     parseMoney[#[[4]]],
     #[[5]],
      #[[6]],
     Hyperlink[#[[7]]]
     } &
   ,
   table
   ];

parseTime[s_String] :=
  Which[
   StringMatchQ[s, 
    DatePattern[{"Hour", "Minute", "AMPM"}, ":" | Whitespace]],
   TimeObject@
    ToExpression@
     With[{splt = StringSplit[s, {":", " "}]}, 
      If[Last[splt] == "PM", {Mod[ToExpression@splt[[1]] + 12, 24], 
        splt[[2]]}, splt[[;; 2]]]],
   True, Interpreter["Time"][s]
   ];

parseMoney[m_String] := Which[
  StringMatchQ[m, "$" ~~ (DigitCharacter | ".") ..],
  Quantity[ToExpression[StringDelete[m, "$"]], "USDollars"],
  True, Interpreter["CurrencyAmount"][m]
  ]

updateData[Dynamic[data_]] :=
  DynamicModule[
   {
    site, table
    },
   
   site = site = Import["https://giveawaylisting.com/", "XMLObject"];
   table = scrapeGiveaway[site];
   
   data = monitorMap[
     
     {
       parseTime[#[[1]]],
       ToExpression@#[[2]],
       ToExpression@#[[3]],
       parseMoney[#[[4]]],
       #[[5]],
        #[[6]],
       Hyperlink[#[[7]]]
       } &
     ,
     table
     ];
   
   Text[Style["Done", Green, Bold]]
   
   ];
   
   (* set up GUI *)
   
   ClearAll[displayData];
displayData[Dynamic[d_]] :=
 DynamicModule[
  {
   sortby = (#1[[1]] > #2[[1]] &),
   list,
   state = None
   },
  list = d;
  Dynamic[
   niceGridAll[
    Join[
     {{
       Button["Time",
        Switch[
         state,
         "Ascending",
         state = "Descending"; sortby = (#1[[1]] < #2[[1]] &),
         _,
         state = "Ascending"; sortby = (#1[[1]] > #2[[1]] &)
         ]
        ],
       Button["Max",
        Switch[
         state,
         "Ascending",
         state = "Descending"; sortby = (#1[[2]] < #2[[2]] &),
         _,
         state = "Ascending"; sortby = (#1[[2]] > #2[[2]] &)
         ]
        ],
       Button["Odds",
        Switch[
         state,
         "Ascending",
         state = "Descending"; sortby = (#1[[3]] < #2[[3]] &),
         _,
         state = "Ascending"; sortby = (#1[[3]] > #2[[3]] &)
         ]
        ],
       Button["Original Price",
        Switch[
         state,
         "Ascending",
         state = "Descending"; sortby = (#1[[4]] < #2[[4]] &),
         _,
         state = "Ascending"; sortby = (#1[[4]] > #2[[4]] &)
         ]
        ],
       "Name",
       "Seller",
       "URL"
       }},
     Sort[list, sortby]
     ]
    , Background -> {None, {Green}}
    ]
   , SynchronousUpdating -> False
   ]
  ]
  
  
  (* use by 
  
updateData[Dynamic[data]]

Dynamic@displayData[Dynamic[data]]

use updateData to pull in any new giveaways
  
  *)
