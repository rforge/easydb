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
    ID_WRB_SOIL_GROUP   INTEGER  NOT NULL  PRIMARY KEY AUTOINCREMENT, 
    --
    NAME                TEXT     NOT NULL, 
    ABBREV              TEXT     NOT NULL  
);  --

INSERT INTO "WRB_SOIL_GROUP" VALUES(1,'AC','Acrisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(2,'AB','Albeluvisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(3,'AL','Alisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(4,'AN','Andosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(5,'AT','Anthrosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(6,'AR','Arenosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(7,'CL','Calcisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(8,'CM','Cambisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(9,'CH','Chernozem');
INSERT INTO "WRB_SOIL_GROUP" VALUES(10,'CR','Cryosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(11,'DU','Durisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(12,'FR','Ferralsol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(13,'FL','Fluvisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(14,'GL','Gleysol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(15,'GY','Gypsisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(16,'HS','Histosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(17,'KS','Kastanozem');
INSERT INTO "WRB_SOIL_GROUP" VALUES(18,'LP','Leptosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(19,'LX','Lixisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(20,'LV','Luvisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(21,'NT','Nitisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(22,'PH','Phaeozem');
INSERT INTO "WRB_SOIL_GROUP" VALUES(23,'PL','Planosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(24,'PT','Plinthosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(25,'PZ','Podzol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(26,'RG','Regosol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(27,'SC','Solonchak');
INSERT INTO "WRB_SOIL_GROUP" VALUES(28,'SN','Solonetz');
INSERT INTO "WRB_SOIL_GROUP" VALUES(29,'UM','Umbrisol');
INSERT INTO "WRB_SOIL_GROUP" VALUES(30,'VR','Vertisol');



-- Table for soil description profiles
CREATE TABLE PROFILE( 
    ID_PROFILE        INTEGER  NOT NULL  PRIMARY KEY AUTOINCREMENT, 
    -- 
    NAME              TEXT     NOT NULL, 
    STUDY             TEXT, 
    ID_WRB_SOIL_GROUP INTEGER, 
    LATITUDE          REAL, 
    LONGITUDE         REAL, 
    COMMENTS          TEXT, 
    FOREIGN KEY(ID_WRB_SOIL_GROUP) REFERENCES WRB_SOIL_GROUP(ID_WRB_SOIL_GROUP) ON DELETE CASCADE ON UPDATE CASCADE 
);  --

INSERT INTO "PROFILE" VALUES(1,'My 1st profile','Study_A',19,59.817899,17.660415,'No comments'); 
INSERT INTO "PROFILE" VALUES(2,'My 2nd profile','Study_A',14,59.814997,17.673934,'No comments'); 




CREATE TABLE HORIZON( 
    ID_HORIZ          INTEGER  NOT NULL  PRIMARY KEY AUTOINCREMENT, 
    ID_PROFILE        INTEGER  NOT NULL, 
    -- 
    HORIZ_NO          INTEGER  NOT NULL, 
    DEPTH_UP          REAL     NOT NULL, 
    DEPTH_LOW         REAL     NOT NULL, 
    PC_CLAY           REAL, 
    PC_SILT           REAL, 
    PC_SAND           REAL, 
    PC_OM             REAL, 
    PH                REAL, 
    FOREIGN KEY(ID_PROFILE) REFERENCES PROFILE(ID_PROFILE) ON DELETE CASCADE ON UPDATE CASCADE, 
    CHECK ((PC_CLAY >= 0) OR (PC_CLAY <= 100)), 
    CHECK ((PC_SILT >= 0) OR (PC_SILT <= 100)), 
    CHECK ((PC_SAND >= 0) OR (PC_SAND <= 100)), 
    CHECK  (PC_OM > 0), 
    CHECK ((PH >= 0) OR (PH <= 14)), 
    CHECK  (HORIZ_NO > 0)  
);  --

-- Create corresponding insert / update / delete triggers (double security)
CREATE TRIGGER FK_INSERT_HORIZON_ID_PROFILE
    BEFORE INSERT ON HORIZON
    FOR EACH ROW BEGIN
        SELECT CASE 
        WHEN ((SELECT ID_PROFILE FROM PROFILE WHERE PROFILE.ID_PROFILE = NEW.ID_PROFILE) IS NULL) 
        THEN RAISE(ROLLBACK, "Insert on table [HORIZON] violates foreign key constraint") 
    END; 
END; 
CREATE TRIGGER FK_UPDATE_HORIZON_ID_PROFILE
    BEFORE UPDATE ON HORIZON
    FOR EACH ROW BEGIN
        SELECT CASE 
        WHEN ((SELECT ID_PROFILE FROM PROFILE WHERE PROFILE.ID_PROFILE = NEW.ID_PROFILE) IS NULL) 
        THEN RAISE(ROLLBACK, "Update on table [HORIZON] violates foreign key constraint") 
    END; 
END;

CREATE TRIGGER FK_DELETE_HORIZON_ID_PROFILE
  BEFORE DELETE ON PROFILE
  FOR EACH ROW BEGIN
      DELETE FROM HORIZON WHERE HORIZON.ID_PROFILE = OLD.ID_PROFILE; 
END;



-- End of the SQL commands
