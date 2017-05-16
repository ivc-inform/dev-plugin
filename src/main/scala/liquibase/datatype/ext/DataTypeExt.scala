package liquibase.datatype.ext

import liquibase.datatype.core.{DateTimeType, IntType}
import liquibase.datatype.{DatabaseDataType, DataTypeInfo}

import liquibase.database.Database
import liquibase.database.core._


@DataTypeInfo(name = "number", aliases = Array("numeric", "java.sql.Types.NUMERIC", "decimal", "java.sql.Types.DECIMAL"), minParameters = 0, maxParameters = 0, priority = 100)
class H2ModNumericType extends liquibase.datatype.LiquibaseDataType {
  override def toDatabaseDataType(database: Database): DatabaseDataType = {
    val dtOverride = new DatabaseDataType("numeric".toUpperCase)
    database match {
      case x: MySQLDatabase => dtOverride
      case x: DB2Database => dtOverride
      case x: MSSQLDatabase => dtOverride
      case x: HsqlDatabase => dtOverride
      case x: DerbyDatabase => dtOverride
      case x: PostgresDatabase => dtOverride
      case x: FirebirdDatabase => dtOverride
      case _ => new DatabaseDataType("number".toUpperCase)
    }
  }
}

@DataTypeInfo(name = "datetime", aliases = Array("java.sql.Types.DATETIME", "java.util.Date", "timestamp"), minParameters = 0, maxParameters = 0, priority = 100)
class H2ModDateTimeType extends DateTimeType


@DataTypeInfo(name = "integer", aliases = Array("int", "java.sql.Types.INTEGER", "java.lang.Integer", "serial"), minParameters = 0, maxParameters = 0, priority = 100)
class H2ModIntType extends IntType

