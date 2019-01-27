
con = dbConnect(SQLite(), "Football_Records.sqlite")
dbListTables(con)
Fixture_Detail =  dbGetQuery(con, "SELECT * FROM Fixture_Detail");head(Fixture_Detail)
Player_Game_Detail =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail");head(Player_Game_Detail)
Player_Metadata =  dbGetQuery(con, "SELECT * FROM Player_Metadata");head(Player_Metadata)
Player_Season =  dbGetQuery(con, "SELECT * FROM Player_Season_Data");head(Player_Season)
<<<<<<< HEAD
dim(Fixture_Detail)
dim(Player_Metadata)
dim(Player_Game_Detail)
dim(Player_Season)

length(unique(Player_Game_Detail$Player))
length(unique(Player_Season$Player_Link))

=======
dim(Fixture_Detail);head(Fixture_Detail)
dim(Player_Game_Detail);head(Player_Game_Detail)
dim(Player_Metadata);head(Player_Metadata)
dim(Player_Metadata);head(Player_Metadata)
>>>>>>> 46da96bd28807f3dce3cd7d6358d5ba06f4c0698
