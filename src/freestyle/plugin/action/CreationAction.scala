package freestyle.plugin.action

import java.util.Properties

import com.intellij.ide.actions.{CreateFileFromTemplateDialog, CreateTemplateInPackageAction}
import com.intellij.ide.fileTemplates.{FileTemplate, FileTemplateManager, JavaTemplateUtil}
import com.intellij.openapi.actionSystem._
import com.intellij.openapi.module.{Module, ModuleType}
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.ui.InputValidatorEx
import com.intellij.openapi.util.IconLoader
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi._
import com.intellij.psi.codeStyle.CodeStyleManager
import org.jetbrains.annotations.NonNls
import org.jetbrains.jps.model.java.JavaModuleSourceRootTypes
import org.jetbrains.plugins.scala.ScalaFileType
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.project._
import org.jetbrains.sbt.project.module.SbtModuleType

/**
 * https://github.com/AdrianRaFo
 */
class CreationAction
    extends CreateTemplateInPackageAction[PsiFile](
      "New Freestyle Algebra",
      "Creates new Freestyle Algebra",
      IconLoader.getIcon("/images/logo.png"),
      JavaModuleSourceRootTypes.SOURCES)
    with DumbAware {

  protected def buildDialog(
      project: Project,
      directory: PsiDirectory,
      builder: CreateFileFromTemplateDialog.Builder) {
    builder.addKind("Algebra", null, "Freestyle Algebra")
    builder.addKind("Tagless", null, "Freestyle Tagless")
    builder.addKind("Module", null, "Freestyle Module")
    builder.setTitle("Create New Freestyle Algebra")
    builder.setValidator(new InputValidatorEx {
      def getErrorText(inputString: String): String = {
        if (inputString.length > 0 && !ScalaNamesUtil.isQualifiedName(inputString)) {
          return "This is not a valid Scala qualified name"
        }
        null
      }

      def checkInput(inputString: String): Boolean =
        true

      def canClose(inputString: String): Boolean =
        !StringUtil.isEmptyOrSpaces(inputString) && getErrorText(inputString) == null
    })
  }

  def getActionName(directory: PsiDirectory, newName: String, templateName: String): String =
    "New Freestyle Algebra"

  def getNavigationElement(createdElement: PsiFile): PsiElement =
    createdElement

  def doCreate(directory: PsiDirectory, newName: String, templateName: String): PsiFile = {
    createClassFromTemplate(directory, newName, templateName)
  }

  override def isAvailable(dataContext: DataContext): Boolean =
    super.isAvailable(dataContext) && isUnderSourceRoots(dataContext)

  private def isUnderSourceRoots(dataContext: DataContext): Boolean = {
    val module: Module = dataContext.getData(LangDataKeys.MODULE.getName).asInstanceOf[Module]
      if (module == null) false
      else
        ModuleType.get(module) match {
          case _: SbtModuleType => true
          case _                => module.hasScala
        }

  }

  private def createClassFromTemplate(
      directory: PsiDirectory,
      className: String,
      templateName: String,
      parameters: String*): PsiFile =
    CreationAction.createFromTemplate(directory, className, templateName, parameters: _*)

  def checkPackageExists(directory: PsiDirectory): Boolean =
    JavaDirectoryService.getInstance.getPackage(directory) != null
}

object CreationAction {
  @NonNls private[action] val NAME_TEMPLATE_PROPERTY: String          = "NAME"
  @NonNls private[action] val LOW_CASE_NAME_TEMPLATE_PROPERTY: String = "lowCaseName"

  def createFromTemplate(
      directory: PsiDirectory,
      name: String,
      templateName: String,
      parameters: String*): PsiFile = {
    val project = directory.getProject
    val template: FileTemplate =
      FileTemplateManager.getInstance(project).getInternalTemplate(templateName)
    val properties: Properties = new Properties(
      FileTemplateManager.getInstance(project).getDefaultProperties())

    properties.setProperty(
      FileTemplate.ATTRIBUTE_PACKAGE_NAME,
      ScalaNamesUtil.escapeKeywordsFqn(JavaTemplateUtil.getPackageName(directory)))
      properties.setProperty(NAME_TEMPLATE_PROPERTY, name)
    properties.setProperty(
      LOW_CASE_NAME_TEMPLATE_PROPERTY,
      name.substring(0, 1).toLowerCase + name.substring(1))

    var i: Int = 0
    while (i < parameters.length) {
      {
        properties.setProperty(parameters(i), parameters(i + 1))
      }
      i += 2
    }
    var text: String = null
    try {
      text = template.getText(properties)
    } catch {
      case e: Exception =>
        throw new RuntimeException(
          "Unable to load template for " + FileTemplateManager.getDefaultInstance
            .internalTemplateToSubject(templateName),
          e)
    }
    val factory: PsiFileFactory = PsiFileFactory.getInstance(project)
    val scalaFileType           = ScalaFileType.INSTANCE
    var fileName = name
    if(templateName == "Freestyle Module")
      fileName="modules"
    val file: PsiFile =
      factory.createFileFromText(s"$fileName.${scalaFileType.getDefaultExtension}", scalaFileType, text)
    CodeStyleManager.getInstance(project).reformat(file)
    directory.add(file).asInstanceOf[PsiFile]
  }
}
