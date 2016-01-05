package webCrawler;

import java.io.IOException;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;


public class FocusedCrawling {

	General_Crawler crawler=new General_Crawler();
	//This function is used to check  if the child link of parent has the given keyphrase. 
	public boolean checkString(String webUrl,String keyPhrase) throws IOException
	{
	
	crawler.delay();
		//System.out.println("keyphrase1:"+keyPhrase);
	//	System.out.println("Second call:"+webUrl);
		Document document=Jsoup.connect(webUrl).timeout(0).get();
	//	StringBuffer buffer=new StringBuffer(document.text());
		String htmlText=document.text();
	//	System.out.println(htmlText);
		if(htmlText.matches(".*"+keyPhrase+".*"))
		{
			return true ;
		
		}
		else
		{
			return false;
		}
	
	
	}
	//This function is used to extract relevent urls which have the keyphrase in them.
	public List<LinkStructure> extractFocusedLink(String webUrl,List<LinkStructure> urlList,int depth,String keyPhrase)
	{
		
	crawler.delay();
		try {
		//	System.out.println("First call:"+webUrl);
			Document document=Jsoup.connect(webUrl).timeout(0).get();
			Elements webUrls=document.select("a[href]");
		
			for (Element link:webUrls)
			{
		
			String Url=link.attr("abs:href");
	//		System.out.println(Url);
			//Url=crawler.createLink(Url);
			
			if(crawler.validateLink(Url))
			{
				if(crawler.handleHash(Url))
				{
		//	System.out.println("validate link");
				if(crawler.checkIfExists(Url,urlList))
				{
				//	System.out.println("checkifexists");
					if(checkString(Url, keyPhrase))
					{
					if(urlList.size()<=999)
					{
				//		System.out.println("In last if loop");
					urlList=crawler.storeLink(Url,urlList,depth);
					System.out.println(Url);
					
				
					
				
					}
				}
				}
			}
			}
			}
			
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	//	System.out.println(urlList.size());
		return urlList;
		
	}
	
		
}
