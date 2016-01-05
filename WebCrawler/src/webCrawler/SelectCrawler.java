package webCrawler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class SelectCrawler {
	
	//Main function to run program.
	public static void main(String args[]) throws IOException
	{
	Crawler crawler=new Crawler();
	FocusedCrawling focusedCrawling=new FocusedCrawling();
	General_Crawler general_Crawler=new General_Crawler();
	String firstUrl="https://en.wikipedia.org/wiki/Hugh_of_Saint-Cher";
	LinkStructure linkStructure=new LinkStructure(1, firstUrl);
	
	String keyphrase;
	System.out.println("Enter the optional keyphrase:");
	Scanner in=new Scanner(System.in); 
	keyphrase=in.nextLine();
	ArrayList<LinkStructure> list=new ArrayList<LinkStructure>();
    list.add(linkStructure);
    list=(ArrayList<LinkStructure>) crawler.extractLink(firstUrl, list,linkStructure.getDepth());
   //crawler.printList(list);
    System.out.println("keyphrase:"+keyphrase);
    if(keyphrase.isEmpty())
    {
    	System.out.println("In unfocused Crawling");
    for(int i=1;i<999;i++)
    {
    	
    	LinkStructure structure=list.get(i);
    	if(structure.getDepth()>=5)
    	{
    		break;
    	}
    	if(list.size()>999)
    	{
    		break;
    	}
    	list=(ArrayList<LinkStructure>) crawler.extractLink(structure.getLink(),list,structure.getDepth());
    	
    }
    String fileName="unfocused";
    general_Crawler.printList(list,fileName);
	} 
    else
    {
    	System.out.println("In focused Crawling");    	
    	list=(ArrayList<LinkStructure>)focusedCrawling.extractFocusedLink(firstUrl, list,linkStructure.getDepth(),keyphrase);
    	for(int i=1;i<999;i++)
    	    {
    	    	
    	    	LinkStructure structure=list.get(i);
    	    	if(structure.getDepth()>=5)
    	    	{
    	    		break;
    	    	}
    	    	if(list.size()>999)
    	    	{
    	    		break;
    	    	}
    	    	list=(ArrayList<LinkStructure>) focusedCrawling.extractFocusedLink(structure.getLink(),list,structure.getDepth(),keyphrase);
    	    	
    	    }
    	String fileName="focused";    
    	general_Crawler.printList(list,fileName);
    }
	}

		
	}
	
	


