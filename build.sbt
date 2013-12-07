// 
// build.sbt
// 
// Created by Frederic DELBOS - fred.delbos@gmail.com on Dec  6 2013.
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// 

name := "99sp"

version := "1.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test")
