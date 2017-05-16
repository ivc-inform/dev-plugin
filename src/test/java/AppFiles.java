
//import com.ddtek.xmlconverter.ConverterFactory;
//import com.ddtek.xmlconverter.ConverterResolver;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import net.sf.saxon.lib.FeatureKeys;
import net.sf.saxon.lib.Validation;
import org.xml.sax.SAXException;

// Run an XSLT
// The following directories and jars must be in your classpath:
// C:/JOB/fsm-generation/dev-plugin/src/test/java
// C:/Program Files (x86)/Stylus Studio X15 XML Enterprise Suite/Components/XML Converters for Java/lib/XMLConverters.jar
// C:/Program Files (x86)/Stylus Studio X15 XML Enterprise Suite/bin/saxonsa.jar
//

public class AppFiles {

  public static void main(String[] args) throws Exception {

    //ConverterResolver resolver = new ConverterFactory().newResolver();

    String inputUri = "file:///c:/JOB/fsm-generation/dev-plugin/src/test/resources/defs/app/AppFiles.xml";
    Writer outWriter = null;
    Reader copyReader = null;
    File tempFile = null;

    String[] schemaUrls = {
            "file:///c:/JOB/fsm-generation/dev-plugin/src/test/resources/defs/app/schemaApp.xsd",
            "file:///c:/JOB/fsm-generation/dev-plugin/src/test/resources/defs/app/schemaISC.xsd",
    };

    String xsltUrl = "file:///c:/JOB/fsm-generation/dev-plugin/src/test/resources/defs/app/AppFiles.xsl";


    try {
      TransformerFactory tFactory = new com.saxonica.config.EnterpriseTransformerFactory();
      tFactory.setAttribute(FeatureKeys.TREE_MODEL, new Integer(net.sf.saxon.event.Builder.STANDARD_TREE));
      tFactory.setAttribute(FeatureKeys.RECOGNIZE_URI_QUERY_PARAMETERS, Boolean.TRUE);
      tFactory.setAttribute(FeatureKeys.STRIP_WHITESPACE, "ignorable");
      tFactory.setAttribute(FeatureKeys.SCHEMA_VALIDATION, new Integer(Validation.LAX));
      tFactory.setAttribute(FeatureKeys.XSLT_VERSION, "2.0");
      tFactory.setAttribute(FeatureKeys.GENERATE_BYTE_CODE, Boolean.FALSE);
      tFactory.setAttribute(FeatureKeys.VALIDATION_WARNINGS, Boolean.TRUE);
      tFactory.setAttribute(FeatureKeys.XSLT_SCHEMA_AWARE, Boolean.TRUE);
      tFactory.setAttribute(FeatureKeys.ALLOW_EXTERNAL_FUNCTIONS, Boolean.TRUE);

      Transformer transformer = tFactory.newTransformer(new StreamSource(xsltUrl));
      //transformer.setURIResolver(resolver);

      tempFile = File.createTempFile("java", ".xml");
      outWriter = new FileWriter(tempFile);

      transformer.setParameter("dontmerge", "");
      transformer.setParameter("replace", "");
      transformer.setParameter("normalize", "");

      System.out.println();
      System.out.println("XSLT starting.");
      transformer.transform(new StreamSource(inputUri),
              new StreamResult(outWriter));
      System.out.println();
      System.out.println("XSLT finished.");

      // copy the output file to the final output
      outWriter.close();
      outWriter = new OutputStreamWriter(System.out);
      copyReader = new FileReader(tempFile);
      char buffer[] = new char[1000];
      int ret;
      while ((ret = copyReader.read(buffer, 0, buffer.length)) > 0)
        outWriter.write(buffer, 0, ret);
      outWriter.flush();
      copyReader.close();

      runValidation(tempFile.toURI().toString(), schemaUrls);
    } finally {
      if (outWriter != null) outWriter.close();
      if (copyReader != null) copyReader.close();
      if (tempFile != null) tempFile.delete();
    }
  }

  private static void runValidation(String dataUrl, String[] schemaUrls) throws SAXException, IOException {

    System.out.println();
    System.out.println("Validation starting.");
    SchemaFactory sFactory = new com.saxonica.jaxp.SchemaFactoryImpl();

    StreamSource[] schemaSources = new StreamSource[schemaUrls.length];
    for (int i = 0; i < schemaUrls.length; i++)
      schemaSources[i] = new StreamSource(schemaUrls[i]);
    Schema combinedSchema = sFactory.newSchema(schemaSources);

    Validator validator = combinedSchema.newValidator();
    validator.validate(new StreamSource(dataUrl));

    System.out.println();
    System.out.println("Validation finished.");
  }

}
