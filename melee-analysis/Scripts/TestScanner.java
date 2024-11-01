package edu.skidmore.indstudy.midi;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class TestScanner {

  public static void main(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
	String fileName = "C:/Users/Shane/Downloads/slippiDataFlat.csv";
	File file = new File(fileName);

	// this gives you a 2-dimensional array of strings
	List<List<String>> lines = new ArrayList<>();
	Scanner inputStream;

	try {
	  inputStream = new Scanner(file);

	  while (inputStream.hasNext()) {
		String line = inputStream.next();
		String[] values = line.split(",");
		// this adds the currently parsed line to the 2-dimensional string array
		lines.add(Arrays.asList(values));
	  }

	  inputStream.close();
	} catch (FileNotFoundException e) {
	  e.printStackTrace();
	}

	// the following code lets you iterate through the 2-dimensional array
	int lineNo = 1;
	for (List<String> line : lines) {
	  int columnNo = 1;
	  for (String value : line) {
		System.out
		    .println("Line " + lineNo + " Column " + columnNo + ": " + value);
		columnNo++;
	  }
	  lineNo++;
	}

	String[][] playerOne = new String[3067][23];
	String[][] playerTwo = new String[3067][23];

	for (int i = 0; i < 3067; i++) {
	  // System.out.println(lines.get(i).get(0));
	  playerOne[i][0] = i + "";
	  playerOne[i][1] = lines.get(i).get(2);
	  playerOne[i][2] = lines.get(i).get(40);
	  playerOne[i][3] = lines.get(i).get(4);
	  playerOne[i][4] = lines.get(i).get(5);
	  playerOne[i][5] = lines.get(i).get(7);
	  playerOne[i][6] = lines.get(i).get(10);
	  playerOne[i][7] = lines.get(i).get(11);
	  playerOne[i][8] = lines.get(i).get(13);
	  playerOne[i][9] = lines.get(i).get(15);
	  playerOne[i][10] = lines.get(i).get(16);
	  playerOne[i][11] = lines.get(i).get(17);
	  playerOne[i][12] = lines.get(i).get(18);
	  playerOne[i][13] = lines.get(i).get(19);
	  playerOne[i][14] = lines.get(i).get(22);
	  playerOne[i][15] = lines.get(i).get(25);
	  playerOne[i][16] = lines.get(i).get(26);
	  playerOne[i][17] = lines.get(i).get(27);
	  playerOne[i][18] = lines.get(i).get(28);
	  playerOne[i][19] = lines.get(i).get(30);
	  playerOne[i][20] = lines.get(i).get(32);
	  playerOne[i][21] = lines.get(i).get(36);
	  playerOne[i][22] = lines.get(i).get(37);
	}

	for (int i = 0; i < 3067; i++) {
	  // System.out.println(lines.get(i).get(0));
	  playerTwo[i][0] = (3067+i) + "";
	  playerTwo[i][1] = lines.get(i).get(29);
	  playerTwo[i][2] = lines.get(i).get(39);
	  playerTwo[i][3] = lines.get(i).get(12);
	  playerTwo[i][4] = lines.get(i).get(38);
	  playerTwo[i][5] = lines.get(i).get(1);
	  playerTwo[i][6] = lines.get(i).get(9);
	  playerTwo[i][7] = lines.get(i).get(31);
	  playerTwo[i][8] = lines.get(i).get(14);
	  playerTwo[i][9] = lines.get(i).get(24);
	  playerTwo[i][10] = lines.get(i).get(3);
	  playerTwo[i][11] = lines.get(i).get(33);
	  playerTwo[i][12] = lines.get(i).get(0);
	  playerTwo[i][13] = lines.get(i).get(34);
	  playerTwo[i][14] = lines.get(i).get(23);
	  playerTwo[i][15] = lines.get(i).get(8);
	  playerTwo[i][16] = lines.get(i).get(20);
	  playerTwo[i][17] = lines.get(i).get(21);
	  playerTwo[i][18] = lines.get(i).get(36);
	  playerTwo[i][19] = lines.get(i).get(30);
	  playerTwo[i][20] = lines.get(i).get(6);
	  playerTwo[i][21] = lines.get(i).get(28);
	  playerTwo[i][22] = lines.get(i).get(35);
	}

	try{
	  PrintWriter writer = new PrintWriter(new FileWriter("slippiData.csv", false));
	
	for (int j = 0; j < 3067; j++) {
	  for (int i = 0; i <= 22; i++) {
		
		  writer.print(playerOne[j][i] + ",");
		
		System.out.print(playerOne[j][i] + ",");
	  }
	  System.out.println();
	  writer.println();
	}
	
	for (int j = 0; j < 3067; j++) {
	  for (int i = 0; i <= 22; i++) {
		
	  		writer.print(playerTwo[j][i] + ",");
		
		System.out.print(playerTwo[j][i] + ",");
	  }
	  System.out.println();
	  writer.println();
	}
	
	writer.close();
	
  }catch(Throwable throwable){
	throwable.printStackTrace();
  }
  }
  
  

}