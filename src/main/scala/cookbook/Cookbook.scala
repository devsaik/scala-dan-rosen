package cookbook

/**
 * Created by rca733 on 3/21/17.
 */
sealed abstract class Mass extends Ordered[Mass]{
  def amount : Double
  def toGrams:Grams
  def compare(that:Mass)= (this.toGrams.amount- that.toGrams.amount).toInt
  //def toString

}
case class Grams(amount : Double) extends Mass{
  override def toGrams = this
  override def toString = amount +"grams"
}
case class KiloGrams(amount : Double) extends Mass{
  override def toGrams = Grams(amount*1000)
  override def toString = amount +"KiloGrams"
  //override def toString = "Kg"
}
case class Recipe(ingredients:Map[String,Mass],directions:List[String]){
 def shoppingList(kitchen: Map[String,Mass]):List[String]=
 for{
   (name , need) <- ingredients.toList
   have = kitchen.getOrElse(name, Grams(0))
   if(have < need)
 }
 yield name

}
class Cookbook {
 var recipes:Map[String,Recipe]= Map.empty
}
