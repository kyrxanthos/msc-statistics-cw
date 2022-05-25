import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.regression.LinearRegressionModel
import org.apache.spark.mllib.regression.LinearRegressionWithSGD
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.evaluation.RegressionMetrics
import org.apache.spark.mllib.stat.{MultivariateStatisticalSummary, Statistics}

val cars_file = sc.textFile("hdfs:///user/user434/coursework/car_data.csv")

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
    val price = pieces(3).trim().toDouble
    val year = pieces(2).trim().toDouble
    val milage = pieces(5).trim().toDouble
    val engineSize = pieces(7).trim().toDouble
    val brand = pieces(8).trim()
    val transmission = pieces(4).trim()
    val fueltype = pieces(6).trim()
    CarsFeatures(price, year, milage, engineSize, brand, transmission, fueltype)
}

val cars = cars_file.filter(x => !isHeader(x)).filter(x => !isOutlier(x)).map(parse)

// explore some correlations
val check_cars = cars.map(x => (x.price ,x.year, x.engineSize, x.milage))
val correlation = Statistics.corr(check_cars.map(_._1),check_cars.map(_._2),"pearson")
val correlation = Statistics.corr(check_cars.map(_._1),check_cars.map(_._3),"pearson")
val correlation = Statistics.corr(check_cars.map(_._1),check_cars.map(_._4),"pearson")


// one hot encodings
val indexed_brand = cars.map(x=>x.brand).distinct().sortBy[String](x=>x).zipWithIndex().collectAsMap()

val indexed_transmission = cars.map(x=>x.transmission).distinct().sortBy[String](x=>x).zipWithIndex().collectAsMap()

val indexed_fuel = cars.map(x=>x.fueltype).distinct().sortBy[String](x=>x).zipWithIndex().collectAsMap()


def encode_brand(x: String) =
 {
    var encodeArray = Array.fill(12)(0)
    encodeArray(indexed_brand.get(x).get.toInt)=1
    encodeArray
 }

def encode_transmission(x: String) =
 {
    var encodeArray = Array.fill(5)(0)
    encodeArray(indexed_transmission.get(x).get.toInt)=1
    encodeArray
 }

def encode_fuel(x: String) =
 {
    var encodeArray = Array.fill(6)(0)
    encodeArray(indexed_fuel.get(x).get.toInt)=1
    encodeArray
 }

val encode_cars = cars.map{ x => (x.price,x.year,x.milage,x.engineSize ,
                                    encode_brand(x.brand), encode_transmission(x.transmission), 
                                    encode_fuel(x.fueltype))}


val new_encode_cars = encode_cars.map(x => ((log(x._1)), Seq(x._2, x._3, x._4, x._5(1), x._5(1)
                                            , x._5(2), x._5(3), x._5(4), x._5(5), x._5(6)
                                            , x._5(7), x._5(8), x._5(9), x._5(10), x._5(11)
                                            , x._6(1), x._6(2), x._6(3), x._6(4)
                                            , x._7(1), x._7(2), x._7(3), x._7(4), x._7(5) )))

val parsedData = new_encode_cars.map{case (k, vs) => LabeledPoint(k, Vectors.dense(vs.toArray))}.cache()

// step size should be small enough
val numIterations = 500
val stepSize = 0.00000001

// define linear regression
val algorithm = new LinearRegressionWithSGD()

// use intercept
algorithm.setIntercept(true).optimizer.setNumIterations(numIterations).setStepSize(stepSize)

// fit model
val model = algorithm.run(parsedData)

val pred_actual = parsedData.map { labeledPoint =>
    // make predictions using the model
    val prediction = model.predict(labeledPoint.features)
    (prediction, labeledPoint.label)
}

val metrics = new RegressionMetrics(pred_actual)

// Root Mean Squared error
println(s"RMSE = ${metrics.rootMeanSquaredError}")
// RMSE = 0.2505

println(s"R-squared = ${metrics.r2}")
// R-squared = 0.78

// create dataframe to move data to local
val pred_df = pred_actual.toDF()
// write dataframe
pred_df.coalesce(1).write.format("com.databricks.spark.csv").save("my_pred_data")

// hadoop fs -get my_pred_data
// scp -r user434@athena.ma.ic.ac.uk://home/user434/bd-sp-2017/data/my_pred_data results/