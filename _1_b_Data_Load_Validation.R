
con = dbConnect(SQLite(), "Football_Records.sqlite")
dbListTables(con)
Fixture_Detail =  dbGetQuery(con, "SELECT * FROM Fixture_Detail");min(table(Fixture_Detail$Season_ID)) == max(table(Fixture_Detail$Season_ID))
Player_Game_Detail =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail");head(Player_Game_Detail)
Player_Metadata =  dbGetQuery(con, "SELECT * FROM Player_Metadata");head(Player_Metadata)
Player_Season =  dbGetQuery(con, "SELECT * FROM Player_Season_Data");head(Player_Season)
dim(Fixture_Detail)
dim(Player_Metadata)
dim(Player_Game_Detail)
dim(Player_Season)
table(Player_Season$Season)

length(unique(Player_Game_Detail$Player))
length(unique(Player_Season$Player_Link))

dim(Fixture_Detail);head(Fixture_Detail)
dim(Player_Game_Detail);head(Player_Game_Detail)
dim(Player_Metadata);head(Player_Metadata)
dim(Player_Metadata);head(Player_Metadata)

dbRemoveTable(con, "Player_Season_Data")
