(* Get data and setup *)

<< MyFunctionLibrary`

scrapeGiveaway[page_] := Cases[
     page,
     
     XMLElement["tr", _,
      {
            XMLElement["td", _, {XMLElement["a", {_, "href" -> url_, _}, {time_}]}],
            XMLElement["td", _, {odds_}],
            XMLElement["td", _, {max_}], XMLElement["td", _, {OGPrice_}],
            _,
            XMLElement["td", _, {XMLElement["a", _, {item_}]}],
            XMLElement["td", _, {seller_}]
       }
     ]
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
     
   ,Infinity
] //. XMLElement[__, x_] :> x /. {x_String} :> x;

parseTime[s_String] :=
     Which[
          StringMatchQ[s, DatePattern[{"Hour", "Minute", "AMPM"}, ":" | Whitespace]],
     
          TimeObject@ToExpression@
               With[
                    {splt = StringSplit[s, {":", " "}]}, 
                    If[Last[splt] == "PM", {Mod[ToExpression@splt[[1]] + 12, 24], splt[[2]]}, splt[[;; 2]]]
                    ],
     
          True, 
          Interpreter["Time"][s]
     ];

parseMoney[m_String] := 
     Which[
          StringMatchQ[m, "$" ~~ (DigitCharacter | ".") ..],
          Quantity[ToExpression[StringDelete[m, "$"]], "USDollars"],
          
          True, 
          Interpreter["CurrencyAmount"][m]
     ]

updateData[Dynamic[data_]] :=
     DynamicModule[
          {
          site, table
          },

          site = Import["https://giveawaylisting.com/", "XMLObject"];
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

(* get initial values *)

site = Import["https://giveawaylisting.com/", "XMLObject"];

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
   
(* set up GUI *)

ClearAll[displayData];
displayData[Dynamic[d_]] :=
DynamicModule[
     {
     sortby = None,
     list,
     state = None
     },
     list = d;
     Dynamic[
          niceGridAll[
               Join[
                    {{
                         Button[
                              Row[{"Time", 
                              Dynamic@Switch[
                                        state, 
                                        
                                        "TimeAscending", " \[UpArrow]",
                                        
                                        "TimeDescending", " \[DownArrow]",
                                        
                                        _, ""
                                   ]
                                 }
                              ],
                              Switch[
                                   state,
                                   "TimeAscending",
                                   state = "TimeDescending"; sortby = (#1[[1]] < #2[[1]] &),
                                   
                                   _,
                                   state = "TimeAscending"; sortby = (#1[[1]] > #2[[1]] &)
                              ]
                         , Appearance -> "Frameless"
                         ]
                    ,
                         Button[
                              Row[{"Max Prizes", 
                              Dynamic@Switch[
                                        state, 
                                        "MaxAscending", " \[UpArrow]",
                                        
                                       "MaxDescending", " \[DownArrow]",
                                       
                                       _, ""
                                   ]
                                 }                             
                              ],
                         Switch[
                              state,
                              "MaxAscending",
                              state = "MaxDescending"; sortby = (#1[[2]] < #2[[2]] &),
                              
                              _,
                              state = "MaxAscending"; sortby = (#1[[2]] > #2[[2]] &)
                         ]
                         , Appearance -> "Frameless"
                         ]
                 ,
                    
                         Button[
                              Row[{"Odds of Winning (1 in ...)", 
                                   Dynamic@Switch[
                                             state, 
                                             "OddsAscending", " \[UpArrow]",
                                             
                                             "OddsDescending", " \[DownArrow]", 
                                             
                                             _, ""
                                   ]
                                 }
                               ],
                              Switch[
                                   state,
                                   
                                   "OddsAscending",
                                   state = "OddsDescending"; sortby = (#1[[3]] < #2[[3]] &),
                                   
                                   _,
                                   state = "OddsAscending"; sortby = (#1[[3]] > #2[[3]] &)
                              ]
                              , Appearance -> "Frameless"
                         ]
               ,
               
                    Button[
                         Row[{"Original Price", 
                         Dynamic@Switch[
                                   state, 
                                   "PriceAscending", " \[UpArrow]", 
                                   
                                   "PriceDescending", " \[DownArrow]",
                                   
                                   _, ""
                                   ]
                                 }
                                ],
                         Switch[
                         state,
                              "PriceAscending",
                              state = "PriceDescending"; sortby = (#1[[4]] < #2[[4]] &),

                              _,
                              state = "PriceAscending"; sortby = (#1[[4]] > #2[[4]] &)
                              ]
                         , Appearance -> "Frameless"
                    ]
               ,
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
