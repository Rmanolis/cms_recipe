package code.lib
import net.liftweb._
import net.liftweb.mapper._
import common._
import http.S
import sitemap._
import sitemap.Loc._
import code.model._

case class MenuLoc(menu: Menu) {
  lazy val url: String = S.contextPath + menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath + menu.loc.calcDefaultHref
}

object Site {

  val home = MenuLoc(Menu.i("Home") / "index" >> User.AddUserMenusAfter)
  val isLoggedIn = If(() => User.loggedIn_?, "You must be logged in")
  val isAdminLoggedIn = If(() => User.loggedIn_? && User.currentUser.map {
    u => u.superUser.is
  }.getOrElse(false), "You must be logged in")

  val addLanguage = MenuLoc(Menu.i("Add Language") / "admin" / "set" / "addLanguage" >> isAdminLoggedIn)
  private val editLanguageMenu = Menu.param[Language]("Language", "Edit Language",
    Language.find _, _.id.is.toString) / "admin" / "set" / "editLanguage" >> isAdminLoggedIn >> Hidden
  lazy val editLanguageLoc = editLanguageMenu.toLoc
  val crudLanguage = MenuLoc(Menu.i("Languages") / "admin" / "languages" >> isAdminLoggedIn)

  val addLocalization = MenuLoc(Menu.i("Add Localization") / "admin" / "set" / "addLocalization" >> isAdminLoggedIn)
  private val editLocalizationMenu = Menu.param[Localization]("Localization", "Edit Localization",
    Localization.find _, _.id.is.toString) / "admin" / "set" / "editLocalization" >> isAdminLoggedIn >> Hidden
  lazy val editLocalizationLoc = editLocalizationMenu.toLoc
  val crudLocalization = MenuLoc(Menu.i("Localizations") / "admin" / "localizations" >> isAdminLoggedIn)

  val addIngredient = MenuLoc(Menu.i("Add Ingredient") / "admin" / "set" / "addIngredient" >> isAdminLoggedIn)
  private val editIngredientMenu = Menu.param[Ingredient]("Ingredient", "Edit Ingredient",
    Ingredient.find _, _.id.is.toString) / "admin" / "set" / "editIngredient" >> isAdminLoggedIn >> Hidden
  lazy val editIngredientLoc = editIngredientMenu.toLoc
  val crudIngredient = MenuLoc(Menu.i("Ingredients") / "admin" / "ingredients" >> isAdminLoggedIn)

  val addFoodType = MenuLoc(Menu.i("Add Food Type") / "admin" / "set" / "addFoodType" >> isAdminLoggedIn)
  private val editFoodTypeMenu = Menu.param[FoodType]("FoodType", "Edit Food Type",
    FoodType.find _, _.id.is.toString) / "admin" / "set" / "editFoodType" >> isAdminLoggedIn >> Hidden
  lazy val editFoodTypeLoc = editFoodTypeMenu.toLoc
  val crudFoodType = MenuLoc(Menu.i("FoodTypes") / "admin" / "foodTypes" >> isAdminLoggedIn)

  val addCourse = MenuLoc(Menu.i("Add Course") / "admin" / "set" / "addCourse" >> isAdminLoggedIn)
  private val editCourseMenu = Menu.param[Course]("Course", "Edit Course",
    Course.find _, _.id.is.toString) / "admin" / "set" / "editCourse" >> isAdminLoggedIn >> Hidden
  lazy val editCourseLoc = editCourseMenu.toLoc
  val crudCourse = MenuLoc(Menu.i("Courses") / "admin" / "courses" >> isAdminLoggedIn)

  val addTypeOfMeasure = MenuLoc(Menu.i("Add Type Of Measure") / "admin" / "set" / "addTypeOfMeasure" >> isAdminLoggedIn)
  private val editTypeOfMeasureMenu = Menu.param[TypeOfMeasure]("TypeOfMeasure", "Edit Type Of Measure",
    TypeOfMeasure.find _, _.id.is.toString) / "admin" / "set" / "editTypeOfMeasure" >> isAdminLoggedIn >> Hidden
  lazy val editTypeOfMeasureLoc = editTypeOfMeasureMenu.toLoc
  val crudTypeOfMeasure = MenuLoc(Menu.i("Type Of Measures") / "admin" / "typeOfMeasures" >> isAdminLoggedIn)

  val crudUser = MenuLoc(Menu.i("Users") / "admin" / "users" >> isAdminLoggedIn)

  val addRecipe = MenuLoc(Menu.i("Add Recipe") / "user" / "set" / "addRecipe" >> isLoggedIn)
  private val editRecipeMenu = Menu.param[Recipe]("Recipe", "Edit Recipe",
    Recipe.find _, _.id.is.toString) / "user" / "set" / "editRecipe" >> isLoggedIn >> Hidden
  lazy val editRecipeLoc = editRecipeMenu.toLoc
  val crudRecipe = MenuLoc(Menu.i("Recipes") / "user" / "recipes" >> isLoggedIn)
  
  private val addPhotoMenu = Menu.param[Recipe]("Add Photo", "Add Photo",
    Recipe.find _, _.id.is.toString) / "user" / "set" / "addPhoto" >> isLoggedIn >> Hidden
  lazy val addPhotoLoc = addPhotoMenu.toLoc

  val searchByIngredients = MenuLoc(Menu.i("Search by ingredient") / "searchByIngredients")

  private val showRecipeMenu = Menu.param[Recipe]("Show Recipe", "Show Recipe",
    Recipe.find _, _.id.is.toString) / "showRecipe"  >> Hidden
  lazy val showRecipeLoc = showRecipeMenu.toLoc
  
  val chooseRecipes = MenuLoc(Menu.i("Choose Recipes") / "chooseRecipes")
  val neededIngredients = MenuLoc(Menu.i("Needed Ingredients") / "neededIngredients" >> Hidden)
  
  private def menu = List(
    home.menu,
    addLanguage.menu,
    editLanguageMenu,
    crudLanguage.menu,
    addLocalization.menu,
    editLocalizationMenu,
    crudLocalization.menu,
    addFoodType.menu,
    editFoodTypeMenu,
    crudFoodType.menu,
    addIngredient.menu,
    editIngredientMenu,
    crudIngredient.menu,
    addCourse.menu,
    editCourseMenu,
    crudCourse.menu,
    addTypeOfMeasure.menu,
    editTypeOfMeasureMenu,
    crudTypeOfMeasure.menu,
    crudUser.menu,
    addRecipe.menu,
    addPhotoMenu,
    editRecipeMenu,
    crudRecipe.menu,
    searchByIngredients.menu,
    showRecipeMenu,
    chooseRecipes.menu,
    neededIngredients.menu)
  def siteMap = SiteMap(menu: _*)
}