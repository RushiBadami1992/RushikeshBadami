public class Probability {
//use to store positive probability	
	double pos;
// use to store negative probability	
	double neg;
	public double getPos() {
		return pos;
	}
//returns negative probability	
	public double getNeg() {
		return neg;
	}
//returns positive probability	
	public Probability(double pos, double neg) {
		super();
		this.pos = pos;
		this.neg = neg;
	}
	

}