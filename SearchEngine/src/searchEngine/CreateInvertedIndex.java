package searchEngine;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
//Use to create inverted index
public class CreateInvertedIndex {
	public static void main (String args[]) throws IOException, ClassNotFoundException
	{
		CreateInvertedIndex invertedIndex=new CreateInvertedIndex();
		
		
		String file=args[0];
	   HashMap<String,HashMap<Integer,Integer>> documentInformation=new HashMap<String, HashMap<Integer,Integer>>(); 
	   documentInformation=(HashMap<String, HashMap<Integer, Integer>>) invertedIndex.CreateInvertedIndex(documentInformation, file);
	    File fileOne=new File(args[1]);
	    FileOutputStream fos=new FileOutputStream(fileOne);
	    ObjectOutputStream oos=new ObjectOutputStream(fos);
	    oos.writeObject(documentInformation);
	    oos.flush();
	    oos.close();
	    fos.close();
	}
	//Use to read data from corpus file and store in index format
	public Map<String,HashMap<Integer,Integer>>  CreateInvertedIndex(Map<String,HashMap<Integer,Integer>> documentInformation,String file) throws IOException
	{
		BufferedReader reader=new BufferedReader(new FileReader(file));
		String line;
		Integer documentId=0;
		while ((line=reader.readLine())!= null)
		{
			if (line.startsWith("#"))
			{
			 String	token[]=line.split(" ");
			 documentId=Integer.parseInt(token[1]);
			}
			else
			{
			String token[]=line.split(" ");
			for (int i=0;i<token.length;i++)
			{
				String key=token[i];
				if(!(key.matches("[0-9]+")))
				{
				if(documentInformation.containsKey(key))
				{
					HashMap<Integer,Integer> termFrequency=documentInformation.get(key);
					if(termFrequency.containsKey(documentId))
					{
						termFrequency.put(documentId, termFrequency.get(documentId)+1);
						documentInformation.put(key, termFrequency);
					}
					else 
					{
						termFrequency.put(documentId, 1);
						documentInformation.put(key, termFrequency);
					}
				}
				else
				{
					HashMap<Integer,Integer> termFrequency=new HashMap<Integer, Integer>();
					termFrequency.put(documentId, 1);
					documentInformation.put(key, termFrequency);
				}
				}
			}
			}
			
		}
		return documentInformation;
		
	}
	
/*private boolean isNumber(String key)
{
	if(key.matches(".*\\d+.*"))
	return false;
	return true;
	
}*/

}
