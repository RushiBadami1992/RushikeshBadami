package Lucy;


	import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
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

	public class GeneralFunctions {
		
		//Sort list of words according to term frequency
		public  HashMap<String,Long> sortMapByInLinkCount(HashMap<String,Long> map )
		{

			List<Map.Entry<String,Long>> list=new LinkedList<Map.Entry<String,Long>>(map.entrySet());
			list=callComparatorforInlinkCount(list);
			Map<String,Long> sortedMap=new LinkedHashMap<String,Long>();
			for (Iterator<Map.Entry<String,Long>> it= list.iterator();it.hasNext();)
			{
				Map.Entry<String,Long>entry=it.next();
				sortedMap.put(entry.getKey(),entry.getValue());
			}
			return (HashMap<String,Long>) sortedMap;
		}
		
		//Comparator to compare words to sort them.
		private List<Entry<String, Long>> callComparatorforInlinkCount(List<Entry<String, Long>> list) {
			// TODO Auto-generated method stub
			Collections.sort(list,new Comparator<Map.Entry<String,Long>>(){
			public int  compare (Map.Entry<String,Long> order1,Map.Entry<String,Long> order2){
				return(order2.getValue()).compareTo(order1.getValue());
			}});
			return list;
		}
		
		//Write data to a csv file
		public void WriteCSVFile(HashMap<Integer,Double> wordfreq) throws IOException
		    {
		  
		    	
		    	Set setOfKeys = wordfreq.keySet();
		    	
				 Iterator iterator = setOfKeys.iterator();
				 PrintWriter pw = new PrintWriter(new FileWriter("rank.csv",true));
				 String header="DocId"+","+"Score"+","+"Rank\n";
				 
				 pw.write(header);
				 Integer count=1;
				 while (iterator.hasNext()) {
					Integer key = (Integer) iterator.next();
					 String content=key+","+wordfreq.get(key)+","+count+"\n";
					 pw.write(content);
					 count++;;
					 
				 }
				pw.close();
					
				 }
		//Write data to a csv file
		 public void writetocsvfile(HashMap<String,Long>wordfreq,long sum) throws IOException
		 {
			 Set setOfKeys = wordfreq.keySet();
		    
			 Iterator iterator = setOfKeys.iterator();
			 PrintWriter pw = new PrintWriter(new FileWriter("zipslaw1.csv",false));
			 int rank=1;
			 while (iterator.hasNext()) {
				 String key = (String) iterator.next();
				 String content= key +","+ wordfreq.get(key)+","+rank;
				 double probability=(double)wordfreq.get(key)/(double)sum;
				 content=content+","+probability;
				 double logRank=Math.log(rank);
				 double logProbability=Math.log(probability);
				 content=content+","+logRank+","+logProbability+"\n";
				 pw.write(content);
				 rank++;
			 }
			 pw.close();
		 }

	}



