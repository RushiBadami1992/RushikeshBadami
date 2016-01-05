package webCrawler;


import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class General_Crawler {
	//This function is used to add a delay of 1 second between each HTTPS call
	public void delay()
	{
		try {
			TimeUnit.SECONDS.sleep(1);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	//This function is used to validate the url
	public boolean validateLink(String webUrl)
	{
	
		boolean validateLink=true; 
		String removeUrl=webUrl.substring(6,webUrl.length());
		if(removeUrl.matches(".*"+":"+".*"))
		{
		return false;
		}
		else if(webUrl.endsWith("Main_Page"))
		{
		return false;		
		}
		else if(!(webUrl.contains("en")))
				{
				return false;
				}
		else if(!(webUrl.contains("https://en.wikipedia.org/wiki")))
		{
			return false;
		}
		return validateLink;
	}
	
/*	public String createLink(String webUrl)
	{
		if(webUrl.startsWith("/wiki/"))
		{
			webUrl="https://en.wikipedia.org" + webUrl;
		}
		return webUrl;
	}*/
	//Function used to check if the link exists in the crawled list of links
	public boolean checkIfExists(String webUrl,List<LinkStructure> webUrls)
	{
	//	System.out.println("In checkifExists");
		boolean checkLink=true;
		for(int i=0;i<webUrls.size();i++)
		{
			LinkStructure structure=webUrls.get(i);
			if(webUrl.matches(structure.getLink()))
			{
				return false;
			}
			
		}
		return checkLink;
	}
	//Function used to store link in a list which has all the links that were crawled
	public List<LinkStructure> storeLink(String weburl,List<LinkStructure> urlList,int depth)
	{ 
		LinkStructure structure=new LinkStructure(depth+1,weburl);
		urlList.add(structure);
		return urlList;
		
	}
	//This function is used to write the pages crawled in a txt file
	public void printList(List<LinkStructure> webUrls,String fileName) throws IOException
	{
		FileWriter writer= new FileWriter(fileName+".txt");
		for(int i=0;i<webUrls.size();i++)
		{
			LinkStructure structure=webUrls.get(i);
			writer.write(structure.getLink());
			writer.write("\r\n");
		
		}
	}
	
	public boolean handleHash(String webUrl)
	{
		if(webUrl.matches(".*#.*"))
		{
			return false;	
		}
		return true;
	}
		
	}
	

