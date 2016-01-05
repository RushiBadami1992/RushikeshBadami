package webCrawler;

import java.io.IOException;
import java.util.List;



import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;


public class Crawler {
	
	General_Crawler crawler=new General_Crawler();
	//Used to extract urls in unfocussed crawling.
	public List<LinkStructure> extractLink(String webUrl, List<LinkStructure> urlList,int depth)
	{
	
		crawler.delay();
		try {
			
			Document document=Jsoup.connect(webUrl).timeout(0).get();
			Elements webUrls=document.select("a[href]");
			
			for (Element link:webUrls)
			{
			String Url=link.attr("abs:href");
			//System.out.println(Url);
			//Url=crawler.createLink(Url);
		
			if(crawler.validateLink(Url))
			{
			//	System.out.println("validate link");
				if(crawler.handleHash(Url))
				{
				if(crawler.checkIfExists(Url,urlList))
				{
				//	System.out.println("checkifexists");
					if(urlList.size()<=999)
					{
					urlList=crawler.storeLink(Url,urlList,depth);
				//	System.out.println(Url);
					
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
