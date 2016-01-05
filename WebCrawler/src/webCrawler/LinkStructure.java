package webCrawler;
//Data Structure to store link and link depth
public class LinkStructure {
	int depth;
	String link;
	public LinkStructure(int depth, String link) {
		super();
		this.depth = depth;
		this.link = link;
	}
	
	public int getDepth() {
		return depth;
	}
	public String getLink() {
		return link;
	}
	
	

}
