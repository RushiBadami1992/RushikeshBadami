package pagerank;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.plaf.basic.BasicTreeUI.TreeCancelEditingAction;

public class Test {
public static void main(String args[])
	{
		HashSet<String>set=new HashSet<String>();
		set.add("WT01-B01-126 WT01-B01-47 WT01-B01-164 WT01-B01-167 WT01-B01-158 WT01-B01-159 WT01-B01-160 WT01-B01-163 WT01-B01-166 WT01-B01-162 WT01-B01-165 WT01-B01-127 WT01-B01-161 ");
		HashMap<String, Double> testmap=new HashMap<String,Double>();
		testmap.put("A", 1.0);
		testmap.put("B", 3.5);
		testmap.put("C", 6.9);
		HashMap<String,String> secondmap=new HashMap<String, String>();
		secondmap.put("A", "B C");
		secondmap.put("B","A C" );
		secondmap.put("C","A B");
		HashSet<String>adddata=new HashSet<String>();
		adddata.add("A");
		adddata.add("B");
		for(String s: adddata)
		{
			System.out.println(s);
		}
	//	secondmap.put("D","B C");
/*		Set setOfKeys = secondmap.keySet();
		 Iterator iterator = setOfKeys.iterator();
		 while (iterator.hasNext())
		 {
			String key = (String) iterator.next();
			int rank=testmap.get(key);
			System.out.println(rank);
		 }*/
		Set setOfkey=testmap.keySet();
		double i=1;
//		double k=(1/(double)testmap.size());
	//	System.out.println(k);
		Iterator itr=setOfkey.iterator();
		while(itr.hasNext())
		{
			 
			String key=(String)itr.next();
			double rank=(1/(double)3)*i;
		//	System.out.println(rank);
			++i;
		     testmap.put(key,rank);
		     
		}
		Set setOfk=testmap.keySet();
		Iterator it=setOfkey.iterator();
		while(it.hasNext())
		{
			String key=(String)it.next();
		    Double rank=testmap.get(key);
		     System.out.println(rank);
		}
	}

}
