package Lucy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.core.SimpleAnalyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.DocsEnum;
import org.apache.lucene.index.Fields;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.MultiFields;
import org.apache.lucene.index.Term;
import org.apache.lucene.index.Terms;
import org.apache.lucene.index.TermsEnum;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.DocIdSetIterator;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopScoreDocCollector;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Bits;
import org.apache.lucene.util.BytesRef;
import org.apache.lucene.util.Version;

public class HW4 {

    private static Analyzer sAnalyzer = new SimpleAnalyzer(Version.LUCENE_47);
    private IndexWriter writer;
    private ArrayList<File> queue = new ArrayList<File>();
    
    //Main Class to run the search engine.
    public static void main(String[] args) throws IOException {
	System.out.println("Enter the FULL path where the index will be created: (e.g. /Usr/index or c:\\temp\\index)");
	HashMap<String, Long> wordfreq=new HashMap<String, Long>();
	GeneralFunctions functions=new GeneralFunctions();
	String indexLocation = null;
	BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
	String s = br.readLine();

	HW4 indexer = null;
	try {
	    indexLocation = s;
	    indexer = new HW4(s);
	} catch (Exception ex) {
	    System.out.println("Cannot create index..." + ex.getMessage());
	    System.exit(-1);
	}


	// read input from user until he enters q for quit
	
	while (!s.equalsIgnoreCase("q")) {
	    try {
		System.out
			.println("Enter the FULL path to add into the index (q=quit): (e.g. /home/mydir/docs or c:\\Users\\mydir\\docs)");
		System.out
			.println("[Acceptable file types: .xml, .html, .html, .txt]");
		s = br.readLine();
		if (s.equalsIgnoreCase("q")) {
		    break;
		}

		// try to add file into the index
		indexer.indexFileOrDirectory(s);
	    } catch (Exception e) {
		System.out.println("Error indexing " + s + " : "
			+ e.getMessage());
	    }
	}

	
	// closeIndex, otherwise the index is not created
	// ===================================================
	indexer.closeIndex();

	
   
	// Read index file and create a csv document with term frequency and word to plot
	// zip's curve
	System.out.println("Reading Index files");
	System.out.println("Enter the FULL path where the index will be located: (e.g. /Usr/index or c:\\temp\\index)");
	String location = null;
	BufferedReader br1 = new BufferedReader(new InputStreamReader(System.in));
	String index = br1.readLine();
	File indexDirectory = new File(index);
	IndexReader reader = IndexReader.open(FSDirectory.open(indexDirectory));
	System.out.println(reader.maxDoc());
	Fields fields = MultiFields.getFields(reader);
	long sum=0;
    Terms terms = fields.terms("contents");
    TermsEnum iterator = terms.iterator(null);
    BytesRef byteRef = null;
    HashSet<String> hashSet=new HashSet<String>();
    while((byteRef = iterator.next()) != null) {
        String term = new String(byteRef.bytes, byteRef.offset, byteRef.length);
        hashSet.add(term);	
        Term termInstance = new Term("contents",term);
		long termFreq = reader.totalTermFreq(termInstance);
		sum=sum+termFreq;
		wordfreq.put(term,termFreq);
	
    }
   
    if(wordfreq.get("pre")-wordfreq.get("html")>0)
    {
    	wordfreq.put("pre", wordfreq.get("pre")-wordfreq.get("html"));
    	wordfreq.remove("html");
    }
    else if(wordfreq.get("pre")-wordfreq.get("html")<0)
    {
    	wordfreq.put("html",wordfreq.get("html")- wordfreq.get("pre"));
    	wordfreq.remove("pre");
    }
    else
    {
    	wordfreq.remove("pre");
    	wordfreq.remove("html");
    }
    
    System.out.println(wordfreq.size());
	
	wordfreq=functions.sortMapByInLinkCount(wordfreq);
	System.out.println(sum);
	
	//Write data of words there rank and termfrequency to a csv file
	functions.writetocsvfile(wordfreq,sum);
	
	
	//Search for a document in corpus according to the word entered.
	IndexSearcher searcher = new IndexSearcher(reader);
	s = "";
   PrintWriter pw = new PrintWriter(new FileWriter("rank.csv"),true);
	while (!s.equalsIgnoreCase("q")) {
	    try {
	    pw.println("Rank"+","+"Document ID"+","+"Score");	
	    TopScoreDocCollector collector = TopScoreDocCollector.create(5000, true);
	    System.out.println("Enter the search query (q=quit):");
		s = br.readLine();
		
		if (s.equalsIgnoreCase("q")) {
		    break;
		}
		
		Query q = new QueryParser(Version.LUCENE_47, "contents",
				sAnalyzer).parse(s);
		searcher.search(q, collector);
		ScoreDoc[] hits = collector.topDocs().scoreDocs;

		// 4. display results of document found and hits recieved.
		System.out.println("Found " + hits.length + " hits.");
		for (int i = 0; i < 100; ++i) {
		    int docId = hits[i].doc;
		    Document d = searcher.doc(docId);
		    pw.println((i + 1) +","+d.get("filename")+ ", " + hits[i].score);
		    }
		long termFreq=0;
		long docCount=0;
		String tokens[]=s.split(" ");
		for(int i=0;i<tokens.length;i++)
		{
		Term termInstance = new Term("contents", tokens[i]);
		termFreq =termFreq+ reader.totalTermFreq(termInstance);
		docCount = reader.docFreq(termInstance);
		}
		System.out.println(s + " Term Frequency " + termFreq
			+ " - Document Frequency " + docCount);
		
	    } catch (Exception e) {
	    e.printStackTrace();
		System.out.println("Error searching " + s + " : "
			+ e.getMessage());
		break;
	    }
	   
	    
	}
	 pw.close();
	}

   //Constructor for homework 4 class. 
   HW4(String indexDir) throws IOException {

	FSDirectory dir = FSDirectory.open(new File(indexDir));

	IndexWriterConfig config = new IndexWriterConfig(Version.LUCENE_47,
			sAnalyzer);

	writer = new IndexWriter(dir, config);
    }

    //Write Index to a file or directory
   
    public void indexFileOrDirectory(String fileName) throws IOException {
	
	addFiles(new File(fileName));

	int originalNumDocs = writer.numDocs();
	for (File f : queue) {
	    FileReader fr = null;
	    try {
		Document doc = new Document();

		// ===================================================
		// add contents of file
		// ===================================================
		fr = new FileReader(f);
		doc.add(new TextField("contents", fr));
		doc.add(new StringField("path", f.getPath(), Field.Store.YES));
		doc.add(new StringField("filename", f.getName(),
			Field.Store.YES));
		//Write data to a document
		
		writer.addDocument(doc);
		System.out.println("Added: " + f);
	    } catch (Exception e) {
		System.out.println("Could not add: " + f);
	    } finally {
		fr.close();
	    }
	}

	int newNumDocs = writer.numDocs();
	System.out.println("");
	System.out.println("************************");
	System.out
		.println((newNumDocs - originalNumDocs) + " documents added.");
	System.out.println("************************");

	queue.clear();
    }

    private void addFiles(File file) {

	if (!file.exists()) {
	    System.out.println(file + " does not exist.");
	}
	if (file.isDirectory()) {
	    for (File f : file.listFiles()) {
		addFiles(f);
	    }
	} else {
	    String filename = file.getName().toLowerCase();
	    // ===================================================
	    // Only index text files
	    // ===================================================
	    if (filename.endsWith(".htm") || filename.endsWith(".html")
		    || filename.endsWith(".xml") || filename.endsWith(".txt")) {
		queue.add(file);
	    } else {
		System.out.println("Skipped " + filename);
	    }
	}
    }

   
    public void closeIndex() throws IOException {
	writer.close();
    }
}