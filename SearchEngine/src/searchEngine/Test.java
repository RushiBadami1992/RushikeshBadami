package searchEngine;

public class Test {

	public static void main(String args[])
	{
		Integer documentId=0;
		String line="# 2";
		if(line.startsWith("#"))
		{
			String token[]=line.split(" ");
			documentId=Integer.parseInt(token[1]);
		}
		System.out.println(documentId);
		String key="abc99";
		System.out.println(key.matches("[0-9]+"));
		
	}
}
