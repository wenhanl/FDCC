SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `erlang` DEFAULT CHARACTER SET latin1 ;
USE `erlang` ;

-- -----------------------------------------------------
-- Table `erlang`.`nodes`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `erlang`.`nodes` ;

CREATE  TABLE IF NOT EXISTS `erlang`.`nodes` (
  `hostname` TEXT NULL DEFAULT NULL ,
  `host_id` INT(11) NOT NULL AUTO_INCREMENT ,
  `ip_addr` TEXT NULL DEFAULT NULL ,
  PRIMARY KEY (`host_id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `erlang`.`user`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `erlang`.`user` ;

CREATE  TABLE IF NOT EXISTS `erlang`.`user` (
  `user_id` INT(11) NOT NULL AUTO_INCREMENT ,
  `password` TEXT NULL DEFAULT NULL ,
  PRIMARY KEY (`user_id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `erlang`.`job`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `erlang`.`job` ;

CREATE  TABLE IF NOT EXISTS `erlang`.`job` (
  `job_id` INT(11) NOT NULL AUTO_INCREMENT ,
  `loc_of_src` TEXT NULL DEFAULT NULL ,
  `scn_host_id` INT(11) NULL ,
  `user_id` INT(11) NULL ,
  `download_loc` TEXT NULL DEFAULT NULL ,
  INDEX `scn_id` (`scn_host_id` ASC) ,
  INDEX `uid` (`user_id` ASC) ,
  PRIMARY KEY (`job_id`) ,
  CONSTRAINT `job_ibfk_1`
    FOREIGN KEY (`scn_host_id` )
    REFERENCES `erlang`.`nodes` (`host_id` ),
  CONSTRAINT `job_ibfk_2`
    FOREIGN KEY (`user_id` )
    REFERENCES `erlang`.`user` (`user_id` ))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `erlang`.`compile`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `erlang`.`compile` ;

CREATE  TABLE IF NOT EXISTS `erlang`.`compile` (
  `compile_id` INT(11) NOT NULL AUTO_INCREMENT ,
  `job_id` INT(11) NULL DEFAULT '0' ,
  `cn_host_id` INT(11) NULL ,
  `filename` VARCHAR(700) NULL DEFAULT 'Unknown' ,
  `status` TEXT NULL DEFAULT NULL ,
  PRIMARY KEY (`compile_id`) ,
  INDEX `cn_id` (`cn_host_id` ASC) ,
  CONSTRAINT `compile_ibfk_1`
    FOREIGN KEY (`job_id` )
    REFERENCES `erlang`.`job` (`job_id` ),
  CONSTRAINT `compile_ibfk_2`
    FOREIGN KEY (`cn_host_id` )
    REFERENCES `erlang`.`nodes` (`host_id` ))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
