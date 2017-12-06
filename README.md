# Misc-Mathematica
Various code I find useful, or fun. Shared online
## Amazon Giveaway tool
1. I made the Amazon giveaway searcher in Dec 2017 when I found a website that tracked Christmas giveaway deals on amazon. It had a very bare bones interface, so I pulled it into Mathematica so I could add sortable headers and make it computable. To use (assuming the page it scrapes is still online):

   Simply run the file and initialize the GUI. You can pull in updates to the giveaway age at any time by using `updateData`. Note that this uses code from another repository called `MyFunctionLibrary` for pretty formatting and scraping. You can inquire about getting this library, fork it from the owner, or replace `monitorMap` and `niceGridAll` constructs with their less fully featured "factory" functions (Map and Grid). Here is what it currently looks like, in case the site goes down:
   
   <a href="https://imgflip.com/gif/20mnvf"><img src="https://i.imgflip.com/20mnvf.gif" title="made at imgflip.com"/></a>
