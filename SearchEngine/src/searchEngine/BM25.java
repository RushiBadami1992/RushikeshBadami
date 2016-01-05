package searchEngine;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.PrintWriter;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;


//Main file to run BM25
public class BM25 {
	
	public static void main (String args[]) throws ClassNotFoundException, IOException
	{	BM25 bm25=new BM25();
		 bm25.bm25(args[1],args[0],Integer.parseInt(args[2]));
    }
	
	//Use to calculate rank according to BM25
	public double calculaterank(int documentlength,int termfrequency, double averagecorpuslength,int totalDocuments,int wordDocuments,int queryFrequency)
	{
		double k1=1.2;
		double b=0.75;
		double k2=100;
		double B=(1 -b);
		double B1=b*(documentlength/(double)averagecorpuslength);
		double K= k1* (double)(B+B1);
		double numerator=totalDocuments-wordDocuments+0.5;
		double denominator=wordDocuments+0.5;
		double log=numerator/denominator;
		double rank=Math.log(log);
		double firstproduct=((k1+1)*termfrequency)/(double)(K+(double)termfrequency);
		double secondproduct=((k2+1)*queryFrequency)/(double)(k2+(double)queryFrequency);
		rank=rank*firstproduct*secondproduct;
		return rank;
	}
	
	
	//Calculate Rank for each query
	public HashMap<Integer,Double> calculateBM25(HashMap<String,HashMap <Integer,Integer>> indexeor,
			String queryword,HashMap<Integer,Integer>documentLength,HashMap<Integer, 
			Double>rankofdocument,double averagecorpuslength,int totalDocuments,int queryFrequency)
	{
		HashMap<Integer, Integer> documentFrequency=new HashMap<Integer, Integer>();
		//System.out.println("In other function"+queryword);
		documentFrequency=indexeor.get(queryword);
		System.out.println(documentFrequency.size());
		Set setOfKeys = documentFrequency.keySet();
		 Iterator iterator = setOfKeys.iterator();
		 while (iterator.hasNext()) {
			 Integer key = (Integer) iterator.next();
			 
			 int documentlength=documentLength.get(key);
			 int termfrequency=documentFrequency.get(key);
			 int wordDocuments=documentFrequency.size();
			 double rank=calculaterank(documentlength, termfrequency, averagecorpuslength, totalDocuments, wordDocuments, queryFrequency);
			 if(rankofdocument.containsKey(key))
			 {
				 double initialvalue=rankofdocument.get(key);
				 double totalrank=initialvalue+rank;
			     rankofdocument.put(key, totalrank)	 ;			 
			 }
			 else
			 {
			 rankofdocument.put(key, rank);
			 }
		 }
		return rankofdocument;
	}
	
	//Calculate B25 for each query
	public HashMap<Integer,Double>   computeBM25query(HashMap<String,HashMap <Integer,Integer>> indexeor,String queryWord,int totalDocuments,
			double averagecorpuslength,HashMap<Integer,Integer>documentLength,HashMap<Integer, Double>rankofdocument)
	{
		HashMap<String , Integer> querywords=new HashMap<String,Integer>();
		String tokens[]=queryWord.split(" ");
		
		//System.out.println(tokens[0]);
		//System.out.println(tokens[1]);
		for(int i=0;i<tokens.length;i++)
		{
			if(querywords.containsKey(tokens[i]))
			{
				querywords.put(tokens[i],(querywords.get(tokens)+ 1));
			}
			else
			{
				querywords.put(tokens[i], 1);
			}
		}
		Set setOfKeys = querywords.keySet();
		 Iterator iterator = setOfKeys.iterator();
		 while (iterator.hasNext()) {
			 String key = (String) iterator.next();
			 //System.out.println(key);
			 int queryfrequency=querywords.get(key);
		    rankofdocument=calculateBM25(indexeor, key, documentLength, rankofdocument, averagecorpuslength, totalDocuments, queryfrequency);
		 }
		return rankofdocument;
		 
	}
	
	//Main program to get document information and compute BM25
	 public void bm25(String queryfile,String indexorFile,int noofDocuments) throws IOException, ClassNotFoundException
	 {
		 
		 ArrayList<String>getListOfQueries=new ArrayList<String>();
		 HashMap<Integer, Integer>documentLength=new HashMap<Integer,Integer>();
		 HashMap<String,HashMap<Integer,Integer>> documentInformation=new HashMap<String, HashMap<Integer,Integer>>(); 
		 HashMap<Integer, Double> documentRank=new HashMap<Integer,Double>();
		 
		 documentInformation=getIndexorInformation(documentInformation, indexorFile);
		 
		// System.out.println(documentInformation.size());
	//	 documentLength=main.documentLength("tccorpus.txt", documentLength);
		 documentLength=getDocumentLength(documentInformation, documentLength);
		 double averageDocumentLength=calculateAverageDocumentLength(documentLength);
		 getListOfQueries=getQueryList(getListOfQueries, queryfile);
		 int querryId=1;
		 for(int i=0;i<getListOfQueries.size();i++)
		 {
		 String queryWord=getListOfQueries.get(i);
		 System.out.println(queryWord);
		 int totalDocuments=documentLength.size();
		 documentRank=computeBM25query(documentInformation, queryWord, totalDocuments,averageDocumentLength, documentLength,  documentRank);
		 documentRank=sortMap(documentRank);
		 WriteFile(queryWord, documentRank,querryId,noofDocuments);
		 documentRank=new HashMap<Integer, Double>();
		 querryId++;
		 }
		 
	 }
	 
	 
	 //Use to get index information from index.out
	 public HashMap<String,HashMap<Integer,Integer>>getIndexorInformation(HashMap<String,HashMap<Integer,Integer>> documentInformation,String indexorFile ) throws IOException, ClassNotFoundException
	 {
		 File toRead=new File(indexorFile);
	        FileInputStream fis=new FileInputStream(toRead);
	        ObjectInputStream ois=new ObjectInputStream(fis);
	        
	         documentInformation=(HashMap<String,HashMap<Integer,Integer>>)ois.readObject();
	         
	        ois.close();
	        fis.close();
		return documentInformation;
		 
	 }
	 
	 //Use to get document length of each document 
	 public HashMap<Integer,Integer >getDocumentLength(HashMap<String,HashMap<Integer, Integer>> documentInformation,HashMap<Integer,Integer>documentLength)
	 {
		 Set setOfKeys = documentInformation.keySet();
		 Iterator iterator = setOfKeys.iterator();
		
		 while (iterator.hasNext())
		 {
			 String key = (String) iterator.next();
			 HashMap<Integer, Integer> termfrequency=new HashMap<Integer, Integer>();
			 termfrequency=documentInformation.get(key);
			 Set setOfK = termfrequency.keySet();
			 Iterator iter = setOfK.iterator();
			 while (iter.hasNext())
			 {
				 Integer documentid=(Integer)iter.next();
				 int count=termfrequency.get(documentid);
				 if(documentLength.containsKey(documentid))
				 {
					 documentLength.put(documentid,documentLength.get(documentid)+count);
				 }
				 else
				 {
					 documentLength.put(documentid,count);
				 }
			 }
		 }
	 return documentLength;
	 }
	 
	 //Calculate average document length of each document
	 public double calculateAverageDocumentLength(HashMap<Integer, Integer>documentLength)
		{
			Set setOfKeys = documentLength.keySet();
			 Iterator iterator = setOfKeys.iterator();
			 int sum=0;
			 while (iterator.hasNext()) {
				 Integer key = (Integer) iterator.next();
				 sum=sum+documentLength.get(key);
		}
			 double average =sum/(double)documentLength.size();
			 System.out.println(average);
			return average;
			
		}
	 //Use to get query list on which result needs to be run
	 public ArrayList<String> getQueryList(ArrayList<String> getListOfQueries,String file) throws IOException
		{
			BufferedReader reader=new BufferedReader(new FileReader(file));
			String line;
			Integer documentId=0;
			while ((line=reader.readLine())!= null)
			{
				getListOfQueries.add(line);
			}
			return getListOfQueries;
		}
	 
	 //Use to sort document according to document score
	 public  HashMap<Integer,Double> sortMap(HashMap<Integer,Double> map )
		{

			List<Map.Entry<Integer,Double>> list=new LinkedList<Map.Entry<Integer,Double>>(map.entrySet());
			list=callComparator(list);
			Map<Integer,Double> sortedMap=new LinkedHashMap<Integer,Double>();
			for (Iterator<Map.Entry<Integer,Double>> it= list.iterator();it.hasNext();)
			{
				Map.Entry<Integer,Double>entry=it.next();
				sortedMap.put(entry.getKey(),entry.getValue());
			}
			return (HashMap<Integer,Double>) sortedMap;
		}
	 
	 //Use to compare each document
		private List<Entry<Integer, Double>> callComparator(List<Entry<Integer, Double>> list) {
			// TODO Auto-generated method stub
			Collections.sort(list,new Comparator<Map.Entry<Integer,Double>>(){
			public int  compare (Map.Entry<Integer,Double> order1,Map.Entry<Integer,Double> order2){
				return(order2.getValue()).compareTo(order1.getValue());
			}});
			return list;
		}
		
		//Write the computation of BM25 to a file.
	    public void WriteFile(String queryWord,HashMap<Integer,Double> documentRank,int queryId,int numberofdocuments) throws IOException
	    {
	    	String id="Q"+Integer.toString(queryId);
	    	//String content=queryId+"\t"+queryWord;
	    	Set setOfKeys = documentRank.keySet();
	    	System.out.println("Write a file");
			 Iterator iterator = setOfKeys.iterator();
			 PrintWriter pw = new PrintWriter(new FileWriter("results.eval",true));
			 String header="query_id" +"\t\t\t\t"+ "doc_id"+"\t\t\t\t"+"rank"+"\t\t\t\t"+"BM25_score"+"\t\t\t\t"+"system_name"+"\n";
			 //System.out.println("Write a file");
			 pw.write(header);
			 Integer count=1;
			 while (iterator.hasNext()) {
				
				 String rank=count.toString();
				 String content=rank+"\t"+id+"\t"+queryWord;
				 
				 Integer key = (Integer) iterator.next();
				 content=content+"\t"+key.toString()+"\t"+documentRank.get(key).toString()+"\t"+"Rushi_PC"+"\n";
				 pw.write(content);
				 count++;
				 if(numberofdocuments<count)
					 break;
			 }
			pw.close();
				
			 }

		
	}
	

