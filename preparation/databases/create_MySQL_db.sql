-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- Author: Julien MOEYS

-- Institution: SLU, Department of Soil and Environment, 
--              Biogeophysics and Water Quality group 

-- Copyright: Julien MOEYS 

-- Language: SQL syntax, SQLite compatible

-- Software environment: SQLite or R + the RSQLite package

-- Miscelaneous: See SQLite data types here 
--               http://www.sqlite.org/datatype3.html 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --




-- Table of Soil Groups in FAO World Reference Base for Soil Ressources.
-- source: http://en.wikipedia.org/wiki/World_Reference_Base_for_Soil_Resources
CREATE TABLE WRB_SOIL_GROUP( 
    `ID_WRB_SOIL_GROUP`   INTEGER  NOT NULL  PRIMARY KEY AUTO_INCREMENT, 
    --
    `NAME`                TEXT     NOT NULL, 
    `ABBREV`              TEXT     NOT NULL  
);  --

INSERT INTO `WRB_SOIL_GROUP` (`ID_WRB_SOIL_GROUP`,`NAME`,`ABBREV`) VALUES
    (1,'AC','Acrisol'), 
    (2,'AB','Albeluvisol'),
    (3,'AL','Alisol'),
    (4,'AN','Andosol'),
    (5,'AT','Anthrosol'),
    (6,'AR','Arenosol'),
    (7,'CL','Calcisol'),
    (8,'CM','Cambisol'),
    (9,'CH','Chernozem'),
    (10,'CR','Cryosol'),
    (11,'DU','Durisol'),
    (12,'FR','Ferralsol'),
    (13,'FL','Fluvisol'),
    (14,'GL','Gleysol'),
    (15,'GY','Gypsisol'),
    (16,'HS','Histosol'),
    (17,'KS','Kastanozem'),
    (18,'LP','Leptosol'),
    (19,'LX','Lixisol'),
    (20,'LV','Luvisol'),
    (21,'NT','Nitisol'),
    (22,'PH','Phaeozem'),
    (23,'PL','Planosol'),
    (24,'PT','Plinthosol'),
    (25,'PZ','Podzol'),
    (26,'RG','Regosol'),
    (27,'SC','Solonchak'),
    (28,'SN','Solonetz'),
    (29,'UM','Umbrisol'),
    (30,'VR','Vertisol');



-- Table for soil description profiles
CREATE TABLE PROFILE( 
    `ID_PROFILE`        INTEGER  NOT NULL  PRIMARY KEY AUTO_INCREMENT, 
    -- 
    `NAME`              TEXT     NOT NULL, 
    `STUDY`             TEXT, 
    `ID_WRB_SOIL_GROUP` INTEGER, 
    `LATITUDE`          REAL, 
    `LONGITUDE`         REAL, 
    `COMMENTS`          TEXT, 
    FOREIGN KEY(`ID_WRB_SOIL_GROUP`) REFERENCES `WRB_SOIL_GROUP`(`ID_WRB_SOIL_GROUP`) ON DELETE CASCADE ON UPDATE CASCADE 
);  --

INSERT INTO `PROFILE` (`ID_PROFILE`,`NAME`,`STUDY`,`ID_WRB_SOIL_GROUP`,`LATITUDE`,`LONGITUDE`,`COMMENTS`) VALUES
    (1,'My 1st profile','Study_A',19,59.817899,17.660415,'No comments'), 
    (2,'My 2nd profile','Study_A',14,59.814997,17.673934,'No comments'); 




CREATE TABLE HORIZON( 
    `ID_HORIZ`          INTEGER  NOT NULL  PRIMARY KEY AUTO_INCREMENT, 
    `ID_PROFILE`        INTEGER  NOT NULL, 
    -- 
    `HORIZ_NO`          INTEGER  NOT NULL, 
    `DEPTH_UP`          REAL     NOT NULL, 
    `DEPTH_LOW`         REAL     NOT NULL, 
    `PC_CLAY`           REAL, 
    `PC_SILT`           REAL, 
    `PC_SAND`           REAL, 
    `PC_OM`             REAL, 
    `PH`                REAL, 
    FOREIGN KEY(`ID_PROFILE`) REFERENCES `PROFILE`(`ID_PROFILE`) ON DELETE CASCADE ON UPDATE CASCADE, 
    CHECK ((`PC_CLAY` >= 0) OR (`PC_CLAY` <= 100)), 
    CHECK ((`PC_SILT` >= 0) OR (`PC_SILT` <= 100)), 
    CHECK ((`PC_SAND` >= 0) OR (`PC_SAND` <= 100)), 
    CHECK  (`PC_OM` > 0), 
    CHECK ((`PH` >= 0) OR (PH <= 14)), 
    CHECK  (`HORIZ_NO` > 0)  
);  --

-- TO DO: Create triggers like for SQLite



-- End of the SQL commands
