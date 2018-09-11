-- MySQL dump 10.13  Distrib 5.7.19, for osx10.12 (x86_64)
--
-- Host: localhost    Database: misa_db
-- ------------------------------------------------------
-- Server version	5.7.19

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `main_db`
--

DROP TABLE IF EXISTS `main_db`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `main_db` (
  `mid` int(11) DEFAULT NULL,
  `name` varchar(50) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `main_db`
--

LOCK TABLES `main_db` WRITE;
/*!40000 ALTER TABLE `main_db` DISABLE KEYS */;
INSERT INTO `main_db` VALUES (1,'N-Methyl-L-Glutamate'),(2,'L-Carnitine hydrochloride');
/*!40000 ALTER TABLE `main_db` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `msms_db`
--

DROP TABLE IF EXISTS `msms_db`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `msms_db` (
  `msmsid` int(11) DEFAULT NULL,
  `mz` float(10,6) DEFAULT NULL,
  `intensity` float(12,4) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `msms_db`
--

LOCK TABLES `msms_db` WRITE;
/*!40000 ALTER TABLE `msms_db` DISABLE KEYS */;
INSERT INTO `msms_db` VALUES (1000,70.065796,1.3948),(1000,85.028885,3.3004),(1000,98.060463,100.0000),(1000,116.070824,87.1474),(1000,131.033783,2.6641),(1000,144.065445,50.9263),(1000,162.075958,54.1517),(1001,60.082802,12.9333),(1001,85.030502,25.7806),(1001,102.093903,20.3680),(1001,103.041100,47.0770),(1001,162.113007,100.0000);
/*!40000 ALTER TABLE `msms_db` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `spectra_db`
--

DROP TABLE IF EXISTS `spectra_db`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `spectra_db` (
  `mid` int(11) DEFAULT NULL,
  `msmsid` int(11) DEFAULT NULL,
  `precursor` float(10,6) DEFAULT NULL,
  `adducttype` varchar(50) DEFAULT NULL,
  `mode` varchar(10) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `spectra_db`
--

LOCK TABLES `spectra_db` WRITE;
/*!40000 ALTER TABLE `spectra_db` DISABLE KEYS */;
INSERT INTO `spectra_db` VALUES (1,1000,162.076096,'1','+'),(2,1001,162.112900,'1','+');
/*!40000 ALTER TABLE `spectra_db` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-09-11 15:09:31
