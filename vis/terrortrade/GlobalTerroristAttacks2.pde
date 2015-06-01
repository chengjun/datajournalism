String filename = "data.csv";
PImage img;
String[] rawData;
ArrayList<Year> allYears = new ArrayList<Year>();

int currYear = 2008;
int countryNumb = 33;
float[] currX = new float[countryNumb];
float[] currY = new float[countryNumb];
float[] currRadius = new float[countryNumb];

PVector graphCenter = new PVector();



void setup() {
	size(900, 850);
	img = loadImage("bg.png");
	textAlign(CENTER, CENTER);
	processData();
}

void draw() {
	background(255);
	image(img, 0, 50, 900, 850);
	drawGUI();
	for (Year c : allYears) {
		c.display();
	}
}

void mousePressed() {
	for (Year c : allYears) {
		if (dist(mouseX, mouseY, c.button.x, c.button.y) < c.buttonSize/2 + 8) {
			currYear = c.yearNumb;
		}
	}
}

void drawGUI() {
	stroke(239);
	strokeWeight(2);
	line(130,780, 770,780);
}

void processData() {
	graphCenter.x = 450;
	graphCenter.y = 425;
	rawData = loadStrings(filename);
	//println(rawData.length);
	for (int i = 1; i < rawData.length; i+=countryNumb) {

		Year c = new Year();
		String[] firstRow = split(rawData[i], ",");
		c.yearNumb = int(firstRow[0]);
		//println(c.yearNumb);
	
		for (int j = 0; j < countryNumb; ++j) {
			String[] thisRow = split(rawData[i+j], ",");
			c.countries[j] = thisRow[1];
			c.attacks[j] = int(thisRow[2]);
			c.nKill[j] = int(thisRow[3]);
			c.nWound[j] = int(thisRow[4]);
			c.distance[j] = float(thisRow[5]);
			c.investment[j] = float(thisRow[6]);
			//println(thisRow);
		}
		allYears.add(c);
		//println(c.countries);
		// println(c.attacks);
		// println(c.nKill);
		// println(c.nWound);
		// println(c.distance);
	}

	for (int i = 0; i < countryNumb; ++i) {
		currX[i] = graphCenter.x;
		currY[i] = graphCenter.y;
		currRadius[i] = 0;
	}

	for (Year c : allYears) {
		c.setValues();
	}
}
class Year{

	//variables
	int yearNumb;
	String[] countries = new String[countryNumb];
	int[] attacks = new int[countryNumb];
	int[] nKill = new int[countryNumb];
	int[] nWound = new int[countryNumb];
	float[] distance = new float[countryNumb];
	float[] investment = new float[countryNumb];
	float[] countryRadius = new float[countryNumb];
	PVector[] pos = new PVector[countryNumb];
	PVector button = new PVector();
	int buttonSize = 20;
	float easing = 0.05;
	

	//constructor
	Year(){

	}

	//functions
	void display() {
		if(currYear == yearNumb) {
			for (int i = 0; i < countryNumb; ++i) {
				//println(countries[i]);
				currX[i] += (pos[i].x - currX[i]) *easing;
				currY[i] += (pos[i].y - currY[i]) *easing;
				currRadius[i] += (countryRadius[i] - currRadius[i]) * easing;


				if (dist(mouseX, mouseY, currX[i], currY[i]) < currRadius[i]/2) {
					noStroke();
					if (i == 0) {
						fill(230, 0, 18);
						//println("Find China");
						ellipse(currX[i], currY[i], currRadius[i], currRadius[i]);
						
						rectMode(CENTER);
						fill(40, 125); 
						rect(currX[i], currY[i] - currRadius[i]/2 - 48, 100, 76);

						fill(40);
						textSize(24);
						text(countries[i], currX[i], currY[i]);
						fill(255);
						textSize(18);
						textAlign(LEFT, CENTER);
						text("袭击:"+attacks[i], currX[i] - 30, currY[i] - currRadius[i]/2 - 70);
						text("死亡:"+nKill[i], currX[i] - 30, currY[i] - currRadius[i]/2 - 50);
						text("受伤:"+nWound[i], currX[i] - 30, currY[i] - currRadius[i]/2 - 30);
						//text("投资:"+int(investment[i])+"万美元", currX[i] - 90, currY[i] - currRadius[i]/2 - 30);
						textAlign(CENTER, CENTER);
					} else {
						fill(46,167,224);
						ellipse(currX[i], currY[i], currRadius[i], currRadius[i]);
						
						rectMode(CENTER);
						fill(40, 125); 
						rect(currX[i], currY[i] - currRadius[i]/2 - 58, 220, 100);

						fill(40);
						textSize(24);
						text(countries[i], currX[i], currY[i]);
						fill(255);
						textSize(18);
						textAlign(LEFT, CENTER);
						text("袭击:"+attacks[i], currX[i] - 90, currY[i] - currRadius[i]/2 - 90);
						text("死亡:"+nKill[i], currX[i] - 90, currY[i] - currRadius[i]/2 - 70);
						text("受伤:"+nWound[i], currX[i] - 90, currY[i] - currRadius[i]/2 - 50);
						text("投资:"+int(investment[i])+"万美元", currX[i] - 90, currY[i] - currRadius[i]/2 - 30);
						textAlign(CENTER, CENTER);
					}

				} else {

					noStroke();
					if (i == 0) {
						fill(230, 0, 18, 150);
					} else {
						fill(46, 167, 224, 150);
					}
					ellipse(currX[i], currY[i], currRadius[i], currRadius[i]);

					fill(100);
					textSize(16);
					text(countries[i], currX[i], currY[i]+currRadius[i]/2 +10);
				}
				//println(pos[i].x);
				//println(pos[i].y);
			}
			

			fill(171,220,243);
			noStroke();
			ellipse(button.x, button.y, buttonSize+8, buttonSize+8);
			fill(137);
			textSize(22);
			text(yearNumb, button.x, button.y + 30);
		} else {
			if (dist(mouseX, mouseY, button.x, button.y) < buttonSize/2) {
				fill(200);
				noStroke();
				ellipse(button.x, button.y, buttonSize, buttonSize);
			} else {
				fill(239);
				noStroke();
				ellipse(button.x, button.y, buttonSize, buttonSize);
			}
			
			fill(181);
			textSize(18);
			text(yearNumb, button.x, button.y + 30);
		}
	}

	void setValues() {

		float radiusSpace = TWO_PI / (countryNumb-1);

		for (int i = 0; i < distance.length; ++i) {
			pos[i] = new PVector();
			pos[i].x = graphCenter.x + 300*distance[i] * sin(i*radiusSpace);
			pos[i].y = graphCenter.y + 300*distance[i] * cos(i*radiusSpace);
			countryRadius[i] = 4*sqrt(attacks[i]);
		}

		button.y = 780;
		button.x = (yearNumb-2000)*(width-260)/9 +130;
	}

}

