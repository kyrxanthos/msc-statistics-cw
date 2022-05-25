// Q5

import org.joda.time.DateTime

val pro_fixed_file = sc.textFile("hdfs:///user/user434/coursework/prox-fixed.csv")

case class ProxReading(timeStamp: org.joda.time.DateTime, id: String, 
 floorNum: String, zone: String)


def isHeader(line: String): Boolean = {
 line.contains("timestamp")
}

def parse(line: String) = {
    val pieces = line.split(", ")
    val fmt = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
    val timestamp_unformatted = pieces(0)
    val timestamp = fmt.parseDateTime(timestamp_unformatted)
    val id = pieces(2).trim()
    val floorNum = pieces(3).trim()
    val zone = pieces(4).trim()
    ProxReading(timestamp, id, floorNum, zone)
}

val pro_fixed = pro_fixed_file.filter(x => !isHeader(x)).map(parse)

pro_fixed.cache()


// Q6 



val most_visited_location = pro_fixed.map(x => (x.floorNum + " " + x.zone, 1)).reduceByKey(_+_).sortBy(_._2)

most_visited_location.collect().last

> res11: (String, Int) = (2 1,6154)


// Q7




val pro_mobile_file = sc.textFile("hdfs:///user/user434/coursework/prox-mobile.csv")

val pro_fixed_file = sc.textFile("hdfs:///user/user434/coursework/prox-fixed.csv")

case class ProxReading_Mobile(id: String)

def isHeader(line: String): Boolean = {
 line.contains("timestamp")
}


def is_correct_date(line: String): Boolean = {
    line.contains("2016-06-07")
}

def parse2(line: String) = {
    val pieces = line.split(", ")
    val id = pieces(2).trim()
    ProxReading_Mobile(id)
}

val pro_mobile = pro_mobile_file.filter(x => !isHeader(x)).filter(x => is_correct_date(x)).map(parse2)

val pro_fixed_id = pro_fixed_file.filter(x => !isHeader(x)).filter(x => is_correct_date(x)).map(parse2)

pro_mobile.cache()

pro_fixed_id.cache()

val total_counts = pro_mobile.map(x => (x.id , 1)).union(pro_fixed_id.map(x => (x.id , 1))).reduceByKey(_+_).sortBy(_._2)

total_counts.collect().last

> res16: (String, Int) = (fresumir001,64)


// Q9

val bldg_measurements_file = sc.textFile("hdfs:///user/user434/coursework/bldg-measurements.csv")

case class BLDG(date: String, damper_position: Float)

def isHeader(line: String): Boolean = {
 line.contains("Date/Time")
}

def parse(line: String) = {
    val pieces = line.split(", ")
    val timestamp_unformatted = pieces(0)
    val damper_position = pieces(192).trim().toFloat
    BLDG(timestamp_unformatted, damper_position)
}

val bldg_measurements = bldg_measurements_file.filter(x => !isHeader(x)).map(parse).sortBy(_.date)

bldg_measurements.filter(x => x.damper_position == 1).first

> res24: BLDG = BLDG(2016-06-02 15:20:00,1.0)


// Q10
import org.apache.spark.mllib.linalg.Vectors

import org.apache.spark.mllib.stat.{MultivariateStatisticalSummary, Statistics}

val bldg_measurements_file = sc.textFile("hdfs:///user/user434/coursework/bldg-measurements.csv")

val haz_measurements_file = sc.textFile("hdfs:///user/user434/coursework/f2z2-haz.csv")

case class BLDG_HAZ(time: String, pos_conc: Double)

def isHeader(line: String): Boolean = {
 line.contains("Date/Time")
}

def parse_bldg(line: String) = {
    val pieces = line.split(", ")
    val timestamp_unformatted = pieces(0)
    val damper_position = pieces(192).trim().toDouble
    BLDG_HAZ(timestamp_unformatted, damper_position)
    // (timestamp_unformatted, damper_position)
}

def parse_haz(line: String) = {
    val pieces = line.split(", ")
    val timestamp_unformatted = pieces(0)
    val haz_conc = pieces(1).trim().toDouble
    BLDG_HAZ(timestamp_unformatted, haz_conc)
    // (timestamp_unformatted, haz_conc)
}

val damper_position = bldg_measurements_file.filter(x => !isHeader(x)).map(parse_bldg)

val haz_conc = haz_measurements_file.filter(x => !isHeader(x)).map(parse_haz)

val damper_data = damper_position.map(x => (x.time, x.pos_conc))

val haz_data = haz_conc.map(x => (x.time, x.pos_conc))

val combined_position_haz = damper_data.join(haz_data) 

val cph_observations = combined_position_haz.map(_._2._1).map(x => Vectors.dense(x.toDouble)) // type: org.apache.spark.rdd.RDD[org.apache.spark.mllib.linalg.Vector]

val cph_summary: MultivariateStatisticalSummary = Statistics.colStats(cph_observations)

// println(cph_summary.mean)

// > [0.4799809141135106] 

// println(cph_summary.variance)

// > [0.11586135283332658]


val correlation = Statistics.corr(combined_position_haz.map(_._2._1),combined_position_haz.map(_._2._2),"pearson")

> correlation: Double = 0.17852920930396365


// try new things

import spark.implicits._


// val columns = Seq("date", "damp", "haz")

// val df cph_observations.toDF(columns:_*)

// case class Prob(d1: String, dp: Double, d2: String, hz: Double)
// val df = cph_observations.map(_.toArray).map(case Array(d1, dp, d2, hz) => Prob(d1, dp, d2, hz)).toDF()

// import java.io._
// val p = new PrintWriter(new File("my_data.csv"))
// p.write(combined_position_haz) 
// // p.write(hazCorrelString)
// p.close()

// // def writeFile(filename: String, lines: Seq[String]): Unit = {
// def writeFile(filename: String, lines: RDD[(String, (Double, Double))]): Unit = {
//     val file = new File(filename)
//     val bw = new BufferedWriter(new FileWriter(file))
//     for (line <- lines) {
//         bw.write(line)
//     }
//     bw.close()
// }

// // val columns=combined_position_haz.take(1).flatMap(a=>a.keys)

// val resultantDF=combined_position_haz.map{value=>
//       val list=value.values.toList
//       (list(0),list(1),list(2))
//       }.toDF(columns:_*)

// resultantDF.show()

// val data_to_write = combined_position_haz.map((d, p) => d + "," + p)


// combined_position_haz.map(x => x.time)

// val trial = damper_data.map(x => unix_timestamp(x._1, format = "yyyy/MM/dd HH:mm:ss") + "," + x._2)


import spark.implicits._

val df1 = damper_data.toDF("date", "damper_position")
val df2 = haz_data.toDF("date", "haz")

val df_all = df1.join(df2, Seq("date"), "inner").orderBy("date")
df_all.show(false)

// add a unique timestamp
val df_trial = df_all.withColumn("unix_timestamp", unix_timestamp($"date", "yyyy-MM-dd HH:mm:ss"))
df_trial.show(false)


// to single file: this actually works but it saves the folder in HDFS!
df_trial.coalesce(1).write.format("com.databricks.spark.csv").save("my_data")

// then from terminal we do: hadoop fs -get my_data

// and then from local scp -r user434@athena.ma.ic.ac.uk://home/user434/bd-sp-2017/coursework/trial_data results/


// now we add lag
import org.apache.spark.sql.functions.lag
import scala.collection.mutable.ListBuffer

val w = org.apache.spark.sql.expressions.Window.orderBy("date")  

val leadDf = df_all.withColumn("new_col", lag("haz", -230, 0).over(w))

// leadDf.show(false)

// this is same as before
leadDf.stat.corr("damper_position", "haz", "pearson")

//better (0.302)
leadDf.stat.corr("damper_position", "new_col", "pearson")


val my_list = new ListBuffer[Double]()

 for(a<-100 to 250 if a %10 == 0)
{
        val leadDf = df_all.withColumn("new_col", lag("haz", -a, 0).over(w))
        val cor = leadDf.stat.corr("damper_position", "new_col", "pearson")
        my_list +=  cor
        // println(cor)
}

// saved to python file for plotting
my_list 


// now see who was in the server room

val pro_fixed_file = sc.textFile("hdfs:///user/user434/coursework/prox-fixed.csv")


case class ProxReading(timeStamp: org.joda.time.DateTime, id: String, 
 floorNum: String, zone: String)


def isHeader(line: String): Boolean = {
 line.contains("timestamp")
}

def parse(line: String) = {
    val pieces = line.split(", ")
    val fmt = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
    val timestamp_unformatted = pieces(0)
    val timestamp = fmt.parseDateTime(timestamp_unformatted)
    val id = pieces(2).trim()
    val floorNum = pieces(3).trim()
    val zone = pieces(4).trim()
    ProxReading(timestamp, id, floorNum, zone)
}

 val server_room_visits = pro_fixed_file.filter(x => !isHeader(x)).map(parse).filter(x => x.zone == "Server Room")

server_room_visits.collect().foreach(println) 

server_room_visits.map(x => (x.id, 1)).reduceByKey(_+_).collect().foreach(println) 












// Q11

import org.joda.time.DateTime

val pro_fixed_file = sc.textFile("hdfs:///user/user434/coursework/prox-fixed.csv")

val haz_measurements_file = sc.textFile("hdfs:///user/user434/coursework/f2z2-haz.csv")


case class ProxReading(timeStamp: String, id: String, 
 floorNum: String, zone: String)


def isHeader(line: String): Boolean = {
 line.contains("timestamp")
}

def parse(line: String) = {
    val pieces = line.split(", ")
    // val fmt = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
    val timestamp_unformatted = pieces(0).substring(0, 10).trim()
    // val timestamp = fmt.parseDateTime(timestamp_unformatted)
    val id = pieces(2).trim()
    val floorNum = pieces(3).trim()
    val zone = pieces(4).trim()
    ProxReading(timestamp_unformatted, id, floorNum, zone)
}

val pro_fixed = pro_fixed_file.filter(x => !isHeader(x)).map(parse)


pro_fixed.filter(x => x.zone == "Server Room").filter(x => x.timeStamp == "2016-06-10").collect().foreach(println)

> ProxReading(2016-06-10,pyoung001,3,Server Room)
ProxReading(2016-06-10,sflecha001,3,Server Room)
ProxReading(2016-06-10,ncalixto001,3,Server Room)
ProxReading(2016-06-10,lbennett001,3,Server Room)
ProxReading(2016-06-10,csolos001,3,Server Room)


// Q13


import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.regression.LinearRegressionModel
import org.apache.spark.mllib.regression.LinearRegressionWithSGD
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.evaluation.RegressionMetrics
import org.apache.spark.mllib.regression.StreamingLinearRegressionWithSGD
import org.apache.spark.mllib.stat.{MultivariateStatisticalSummary, Statistics}
import org.apache.commons.math3.util.MathUtils


val cars_file = sc.textFile("hdfs:///user/user434/coursework/car_data.csv") //.map(x=>x.split(','))

def isHeader(line: String): Boolean = {
 line.contains("model")
}

def isOutlier(line: String): Boolean = {
    line.contains("2060")
}

case class CarsFeatures(price: Double, year: Double, 
 milage: Double, engineSize: Double, brand: String, transmission: String, fueltype: String)

def parse(line: String) = {
    val pieces = line.split(",")
    // val fmt = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
    val price = pieces(3).trim().toDouble
    val year = pieces(2).trim().toDouble
    val milage = pieces(5).trim().toDouble
    val engineSize = pieces(7).trim().toDouble
    val brand = pieces(8).trim()
    val transmission = pieces(4).trim()
    val fueltype = pieces(6).trim()
    CarsFeatures(price, year, milage, engineSize, brand, transmission, fueltype)
    // (price, year, milage, engineSize, brand)
}

// something is wrong in my parse function I get the error that "price" has the wrong dormat
val cars = cars_file.filter(x => !isHeader(x)).filter(x => !isOutlier(x)).map(parse)

// val trial_cars = cars.map(x => (x.price ,x.year))
val check_cars = cars.map(x => (x.price ,x.year, x.engineSize, x.milage))
val correlation = Statistics.corr(check_cars.map(_._1),check_cars.map(_._2),"pearson")
val correlation = Statistics.corr(check_cars.map(_._1),check_cars.map(_._3),"pearson")
val correlation = Statistics.corr(check_cars.map(_._1),check_cars.map(_._4),"pearson")


val indexed_brand = cars.map(x=>x.brand).distinct().sortBy[String](x=>x).zipWithIndex().collectAsMap()

val indexed_transmission = cars.map(x=>x.transmission).distinct().sortBy[String](x=>x).zipWithIndex().collectAsMap()

val indexed_fuel = cars.map(x=>x.fueltype).distinct().sortBy[String](x=>x).zipWithIndex().collectAsMap()


def encode_brand(x: String) =
 {
    // 11 + 1 
    var encodeArray = Array.fill(12)(0)
    encodeArray(indexed_brand.get(x).get.toInt)=1
    encodeArray
 }

def encode_transmission(x: String) =
 {
    // 4 + 1 
    var encodeArray = Array.fill(5)(0)
    encodeArray(indexed_transmission.get(x).get.toInt)=1
    encodeArray
 }

def encode_fuel(x: String) =
 {
    // 5 + 1 
    var encodeArray = Array.fill(6)(0)
    encodeArray(indexed_fuel.get(x).get.toInt)=1
    encodeArray
 }

val encode_cars = cars.map{ x => (x.price,x.year,x.milage,x.engineSize ,
                                    encode_brand(x.brand), encode_transmission(x.transmission), 
                                    encode_fuel(x.fueltype))}

val just_y = cars.map{x => (x.price)}
val log_y = just_y.map(x => org.apache.commons.math3.analysis.function.Log(2.0, x))

// val new_encode_cars = encode_cars.map(x => ((x._1), Seq(x._2, x._3, x._4, x._5(1), x._5(1)
//                                             , x._5(2), x._5(3), x._5(4), x._5(5), x._5(6)
//                                             , x._5(7), x._5(8), x._5(9), x._5(10), x._5(11)
//                                             , x._6(1), x._6(2), x._6(3), x._6(4)
//                                             , x._7(1), x._7(2), x._7(3), x._7(4), x._7(5) )))

val new_encode_cars = encode_cars.map(x => ((log(x._1)), Seq(x._2, x._3, x._4, x._5(1), x._5(1)
                                            , x._5(2), x._5(3), x._5(4), x._5(5), x._5(6)
                                            , x._5(7), x._5(8), x._5(9), x._5(10), x._5(11)
                                            , x._6(1), x._6(2), x._6(3), x._6(4)
                                            , x._7(1), x._7(2), x._7(3), x._7(4), x._7(5) )))

val new_encode_cars = encode_cars.map(x => ((log(x._1)), (Seq(x._2, x._3))))

// val correlation = Statistics.corr(new_encode_cars.map(_._1),new_encode_cars.map(_._2),"pearson")
// finally: https://stackoverflow.com/a/35986702/17986681 
// val parsedData = encode_cars_trial.map{case (k, vs) => LabeledPoint(k, Vectors.dense(vs.toArray))}.cache()
val parsedData = new_encode_cars.map{case (k, vs) => LabeledPoint(k, Vectors.dense(vs.toArray))}.cache()


val numIterations = 500
val stepSize = 0.00000001


//  THIS STEPSIZE WORKS
// val stepSize = 0.00000001

// val model = LinearRegressionWithSGD.train(parsedData, numIterations,stepSize)

// println(model.intercept)
// println(model.weights) 

val algorithm = new LinearRegressionWithSGD()
// val algorithm = new StreamingLinearRegressionWithSGD()

algorithm.setIntercept(true).optimizer.setNumIterations(numIterations).setStepSize(stepSize)
// algorithm.optimizer.setNumIterations(numIterations).setStepSize(stepSize)


val model = algorithm.run(parsedData)

val pred_actual = parsedData.map { labeledPoint =>
    // The predict() function of a model receives a feature vector,
    // and returns a predicted label value.
    val prediction = model.predict(labeledPoint.features)
    (prediction, labeledPoint.label)
}

val metrics = new RegressionMetrics(pred_actual)
// val metrics = new RegressionMetrics(pred_actual, True)

// Squared error
println(s"MSE = ${metrics.meanSquaredError}")
println(s"RMSE = ${metrics.rootMeanSquaredError}")

println(s"R-squared = ${metrics.r2}")

// Mean absolute error
println(s"MAE = ${metrics.meanAbsoluteError}")

// Explained variance
println(s"Explained variance = ${metrics.explainedVariance}")

// inspect some results
pred_actual.take(5)

val pred_df = pred_actual.toDF()


pred_df.coalesce(1).write.format("com.databricks.spark.csv").save("my_pred_data")
// hadoop fs -get my_pred_data

// scp -r user434@athena.ma.ic.ac.uk://home/user434/bd-sp-2017/data/my_pred_data results/