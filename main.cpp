#include "Client.h"
#include <conio.h>
#include <iostream>
#include <stdio.h>      /* printf, scanf, puts, NULL */
#include <stdlib.h>     /* srand, rand */
#include <time.h>       /* time */
#include <algorithm>    /* copy array */
using namespace std;

class TestClient : public Client
{
public:
	TestClient(string serverName, short int port) : Client("Elyasi", serverName, port)
	{
		myRo = 0;
		opRo = 0;
		opCo = 0;
		myCo = 0;
		IBO = false;
		point_train = train(9);
		for (i = 0; i < 270; i++)
			trans1[i] = *(point_train + i);
		point_train = train(0);
		for (i = 0; i < 270; i++)
			trans2[i] = *(point_train + i);
	}
	virtual ~TestClient() {}
private:
	int myRo;
	int opRo;
	int opCo;
	int myCo;
	bool IBO;
	double trans1[270];
	double trans2[270];
	double DeMatrix[6][9][5];
	double OfMatrix[6][9][5];
	int n, m, j, i = 0;
	double *point_train;
	double * LReP(int i, int beta, int ac, double pk[5], double l1, double l2, int eval);
	double * train(int c);
	int best(int myRo, int myCo, int opRo, int opCo, bool IBO, double trans2[270], double trans1[270]);

protected:
	virtual void Think()
	{
		//int bestAction = rand() % 5;
		int bestAction;
		IBO = IamBallOwner();
		myRo = MyR()-1;
		opRo = OpponentR()-1;
		opCo = OpponentC()-1;
		myCo = MyC()-1;
		bestAction = best(myRo, myCo, opRo, opCo, IBO, trans2,trans1);
		
		cout << bestAction;

		switch (bestAction)
		{
		case 0:	MoveUp();
			break;

		case 1: MoveRight();
			break;

		case 2: MoveDown();
			break;

		case 3: MoveLeft();
			break;

		case 4: Hold();
			break;
		}
		}
	};

	int main()
	{
		try
		{
			TestClient client("127.0.0.1", 5050);
			client.Start();
		}
		catch (std::string reason)
		{
			cerr << "Exception: " << reason << endl;
			getch();
		}

		return 0;
	}


	double * TestClient::LReP(int i, int beta, int ac, double pk[5], double l1, double l2, int eval)
	{
		static double PkNew[5];
		l1 = l1 / eval; /* reward according to steps to door*/
		double PiNew;
		int k = 0;
		int j;
		int counter;
		PiNew = pk[i] - pk[i] * (l1*beta) - pk[i] * (l2*(1 - beta)) + l1*beta;
		double pn[5 - 1];  /* */
		for (j = 0; j < 5 - 1; j++){
			if (j != i) {
				pn[k] = pk[j];
				k += 1;
			}
		}
		k = 0;
		double PjNew[5 - 1];
		for (j = 0; j < (5 - 1); j++){
			double temp;
			temp = pn[j] - pn[j] * (l1*beta) + (pn[j] * (-1) + 1 / (5 - 1))*(l2*(1 - beta));
			pn[j] = temp;
		}
		for (j = 0; j < 5; j++){
			if (j == i)
				PkNew[j] = PiNew;
			else{
				PkNew[j] = PjNew[k];
				k += 1;
			}
		}
		// ********************************************normalization and exploration
		double normal = 0;
		for (counter = 0; counter < 5; counter++){
			if (PkNew[counter] < 0.0001)
				PkNew[counter] = 0.0001;
			normal += PkNew[counter];
		}
		for (counter = 0; counter < 5; counter++)
			PkNew[counter] = PkNew[counter] / normal;
		return PkNew;
	} // end of func LREP



	double * TestClient::train(int c)
	{
		srand(time(NULL));  /* initialize random seed: */
		int n, m, j, i,action=1, counter;                            // loops
		const double landa1 = 0.5;                    // first  learning  parameter
		const double landa2 = 0.001;                  // second learning  parameter
		int beta;                                    //  reinforcement parameter
		double p = 0.9999;                      //  wall probability
		int act[5];                                 // actions
		double normal = 0;
		double pk[5],Pr[5];
		double ptemp[5];
		double ActPr[5] = { (1 / (5 - 1)), (1 / (5 - 1)), (1 / (5 - 1)), (1 / (5 - 1)), 0.001 };
		// ActPr = { [0 ... ac] = (1/(ac-1)) };// actions probabilities
		// make a matrix and populate initial action probabilities on it
		double PMatrix[6][9][5]; // gcc extension = { [0 ... (row-1)][0 ... (column-1)][0 ... (ac-1)] = (1/(ac-1)) };
		static double return_of_func[6 * 9 * 5];
		for (n = 0; n < 6; n++)
		{
			for (m = 0; m < 9; m++)
			{
				for (j = 0; j < (5 - 1); j++)
				{
					if (j != (5 - 1))
						PMatrix[n][m][j] = (1 / (5 - 1));
					else
						PMatrix[n][m][5 - 1] = 0.001;
				}
			}
		}
		for (m = 0; m < 9; m++)
		{
			PMatrix[0][m][0] = 1 - p;       // define top wall
			PMatrix[6 - 1][m][2] = 1 - p;   // define down wall
		}
		for (n = 0; n < 6; n++)
		{
			PMatrix[n][0][3] = 1 - p;       // define left wall
			PMatrix[n][9 - 1][1] = 1 - p;// define right wall
		}
		if (c == 9) {
			PMatrix[2][9 - 1][1] = p;
			PMatrix[3][9 - 1][1] = p;
		}
		// -------------------
		int it = 400;
		int st = 20;
		// --------------------------------------------------------------------------
		for (i = 0; i < it; i++) { // iteration for
			int s = st;
			int eval = 1;
			int rows = rand() % 6;        // random position for train
			int cols = rand() % 9;     // random position for train
			int randrow = rows;
			int randcol = cols;
			int RC = 0; // Route Scope
			int route[3][100];
			int val;
			while (((randrow != 2) && (randrow != 3)) || (randcol != (c))){
				if (s <= 0)
					break;
				// ********************************************fill pk
				for (j = 0; j < 5; j++){
					pk[j] = PMatrix[randrow][randcol][j];
				}
				// ********************************************wall check
				if (randrow == 0)
					pk[0] = pk[0] * (1 - p);
				if (randrow == 6 - 1)
					pk[2] = pk[2] * (1 - p);
				if (randcol == 0)
					pk[3] = pk[3] * (1 - p)*(1 - p);
				if (randcol == 9 - 1)
					pk[1] = pk[1] * (1 - p);
				// ********************************************normalization and exploration
				for (counter = 0; counter < 5; counter++){
					if (pk[counter] < 0.0001)
						pk[counter] = 0.0001;
					normal += pk[counter];
				}
				for (counter = 0; counter < 5; counter++)
					pk[counter] = pk[counter] / normal;
				// ******************************************** select random with probability
				srand(time(NULL));
				val = rand() % 32000;
				ptemp[0] = pk[0]*32000;
				for (counter = 1; counter < 5 ; counter++)
					ptemp[counter] = ptemp[counter-1] +pk[counter]*32000 ;
				for (counter = 0; counter < 5; counter++) {
					if (val < ptemp[counter]){
						action = counter;
						break;
					}
					val -= ptemp[counter];
				}
				// ******************************************** save route
				route[0][RC] = randrow;
				route[1][RC] = randcol;
				route[2][RC] = action;
				switch (action) {
				case 0: if (randrow != 0)
					randrow = randrow - 1;
					break;
				case 3: if (randcol != 0)
					randcol = randcol - 1;
					break;
				case 2: if (randrow != 6 - 1)
					randrow = randrow + 1;
					break;
				case 1: if (randcol != 9 - 1)
					randcol = randcol + 1;
					break;
				}    // end switch
				// ---------------Update & check battery charge
				s = s - 1;
				RC = RC + 1;
			}       // end while
			if (((randrow != 2) && (randrow != 3)) || (randcol != c ))
				beta = 0;  //  fail
			else{
				beta = 1; // success
				eval = RC;
			}
			double *point_LReP;
			for (m = 0; m < RC; m++) {  // update actions in route with reward and penalty
				for (j = 0; j < 5; j++)
					pk[j] = PMatrix[route[0][m]][route[1][m]][j];
				point_LReP = LReP(route[2][m], beta, 5, pk, landa1, landa2, eval);
				for (j = 0; j < 5; j++)
					Pr[j] = *(point_LReP + j);
				for (j = 0; j < 5; j++)
					PMatrix[route[0][m]][route[1][m]][j] = Pr[j];
			} // end update probabilities for
		}// end iteration for
		i = 0;
		for (n = 0; n < 6; n++)
		{
			for (m = 0; m < 9; m++)
			{
				for (j = 0; j < 5; j++)
				{
					return_of_func[i] = PMatrix[n][m][j];
					i += 1;
				}
			}
		}
		return return_of_func;
	}// end function train


	int TestClient::best(int myRo, int myCo, int opRo, int opCo, bool IBO, double trans2[270], double trans1[270])
	{
		int action = 4,counter,val;
		int n, m, j, i = 0, beta;
		const double landa1 = 0.5;                    // first  learning  parameter
		const double landa2 = 0.001;                  // second learning  parameter
		double p = 0.9999;                            //  wall probability
		int act[5];                                 // actions
		double temp[6 * 9 * 5];
		double *point_train;
		double OfMatrix[6][9][5];
		double DeMatrix[6][9][5];
		double Pr[5], pk[5], ptemp[5], *point_LReP, *point_LReP1, *point_LReP2, normal;
		int randrow, randcol, RC, eval, s, st = 20, it = 40, route[3][100];
		bool imballowner = false;
		srand(time(NULL));

		for (n = 0; n<6; n++)
			for (m = 0; m<9; m++)
				for (j = 0; j<5; j++){
					DeMatrix[n][m][j] = trans2[i];
					i += 1;
					}
		i = 0;
		for (n = 0; n<6; n++)
		for (m = 0; m<9; m++)
		for (j = 0; j<5; j++){
			OfMatrix[n][m][j] = trans1[i];
			i += 1;
		}
/* 
		for (n = 0; n < 6; n++)    
			for (m = 0; m < 9; m++)
				for (j = 0; j < (5); j++)
				{
					if (j != (5 - 1)){
						OfMatrix[n][m][j] = (1 / (5 - 1));
						DeMatrix[n][m][j] = (1 / (5 - 1));
					}
					else{
						OfMatrix[n][m][5 - 1] = 0.0001;
						DeMatrix[n][m][5 - 1] = 0.0001;
					}
				}*/
		/*point_train = train(9);
		for (n = 0; n < 6; n++)
		{
			for (m = 0; m < 9; m++)
			{
				for (j = 0; j < 5; j++)
				{
					OfMatrix[n][m][j] = *(point_train + i);
					i += 1;
				}
			}
		}
		point_train = train(1);
		i = 0;
		for (n = 0; n < 6; n++)
		{
			for (m = 0; m < 9; m++)
			{
				for (j = 0; j < 5; j++)
				{
					DeMatrix[n][m][j] = *(point_train + i);
					i += 1;
				}
			}
		}*/
		if (IBO){
			// --------------------------------------------------------------------------
			for (i = 0; i < it; i++) { // iteration for
				s = st;
				eval = 1;
				randrow = myRo;
				randcol = myCo;
				RC = 0; // Route Scope
				route[3][100] = {};
				while (randcol != (9)){
					if (s <= 0)
						break;
					// ********************************************fill pk
					for (j = 0; j < 5; j++)
						pk[j] = OfMatrix[randrow][randcol][j];
					// ********************************************neighbors check
					if ((randrow - 1) == opRo && (randcol) == opCo)
						pk[0] = pk[0] * (1 - p);
					if ((randrow + 1) == opRo && (randcol) == opCo)
						pk[2] = pk[2] * (1 - p);
					if ((randrow) == opRo && (randcol - 1) == opCo)
						pk[3] = pk[3] * (1 - p)*(1 - p);
					if (randrow == opRo && (((randcol + 1) == opCo) || (randcol + 2) == opCo))
						pk[1] = pk[1] * (1 - p);
					// ********************************************normalization and exploration
					normal = 0;
					for (counter = 0; counter < 5; counter++){
						if (pk[counter] < 0.0001)
							pk[counter] = 0.0001;
						normal += pk[counter];
					}
					for (counter = 0; counter < 5; counter++)
						pk[counter] = pk[counter] / normal;
					// ******************************************** select random with probability
					val = rand() % 32000;
					ptemp[0] = pk[0]*32000;
					for (counter = 1; counter < 5 ; counter++)
						ptemp[counter] = ptemp[counter-1] + pk[counter] * 32000;
					for (counter = 0; counter < 5; counter++) {
						if (val < ptemp[counter]){
							action = counter;
							break;
						}
						val -= ptemp[counter];
					}
					// ******************************************** save route
					route[0][RC] = randrow;
					route[1][RC] = randcol;
					route[2][RC] = action;
					int visitedb = 0;  // opp observed
					switch (action) {
					case 0: if (randrow != 0){
								if ((randrow - 1) == opRo && randcol == opCo)
									visitedb = 1;
								randrow = randrow - (1 - visitedb);
					}
							break;
					case 3: if (randcol != 0){
								if ((randrow) == opRo && (randcol - 1) == opCo)
									visitedb = 1;
								randcol = randcol - (1 - visitedb);
					}
							break;
					case 2: if (randrow != 6 - 1){
								if ((randrow + 1) == opRo && randcol == opCo)
									visitedb = 1;
								randrow = randrow + (1 - visitedb);
					}
							break;
					case 1: if ((randrow == 2) || (randrow == 3) || (randcol != 8)){
								if (randrow == opRo && (randcol + 1) == opCo)
									visitedb = 1;
								randcol = randcol + (1 - visitedb);
					}
							break;
					}    // end switch
					// ---------------Update
					s = s - 1;
					RC = RC + 1;
				}       // end while
				if (randcol != 9)
					beta = 0;  //  fail
				else{
					beta = 1; // success
					eval = RC;
				}
				for (m = 0; m < RC; m++) {  // update actions in route with reward and penalty
					for (j = 0; j < 5; j++)
						pk[j] = OfMatrix[route[0][m]][route[1][m]][j];
					point_LReP1 = LReP(route[2][m], beta, 5, pk, landa1, landa2, eval);
					for (j = 0; j < 5; j++)
						Pr[j] = *(point_LReP1 + j);
					for (j = 0; j < 5; j++)
						OfMatrix[route[0][m]][route[1][m]][j] = Pr[j];
				} // end update probabilities for
			}// end iteration for

			// ******************************************** make action
			randrow = myRo;
			randcol = myCo;
			for (j = 0; j < 5; j++)
				pk[j] = OfMatrix[randrow][randcol][j];
			// Wall check*********************
			if (randrow == 0)
				pk[0] = pk[0] * (1 - p);
			if (randrow == 6 - 1)
				pk[2] = pk[2] * (1 - p);
			if ((randcol == 9 - 1) && ((randrow != 2) && (randrow != 3)))
				pk[1] = pk[1] * (1 - p);
			if (randcol == 0)
				pk[3] = pk[3] * (1 - p);
			// ********************************************check neighbors
			if ((randrow - 1) == opRo && (randcol) == opCo)
				pk[0] = pk[0] * (1 - p);
			if ((randrow) == opRo && (randcol - 1) == opCo)
				pk[3] = pk[3] * (1 - p);
			if ((randrow + 1) == opRo && (randcol) == opCo)
				pk[2] = pk[2] * (1 - p);
			if ((randrow) == opRo && (randcol + 1) == opCo)
				pk[1] = pk[1] * (1 - p);
			// ********************************************normalization and exploration
			normal = 0;
			for (counter = 0; counter < 5 ; counter++){
				if (pk[counter] < 0.0001)
					pk[counter] = 0.0001;
				normal += pk[counter];
			}
			for (counter = 0; counter < 5 ; counter++)
				pk[counter] = pk[counter] / normal;

			// ********************************************
			ptemp[0] = pk[0] * 32000;
			val = rand() % 32000;
			for (counter = 1; counter < 5 ; counter++)
				ptemp[counter] = ptemp[counter-1] + pk[counter] * 32000;
			for (counter = 0; counter < 5; counter++) {
				if (val < ptemp[counter]){
					action = counter;
					break;
				}
				val -= ptemp[counter];
			}
			//********************************************
		}
		else {
			for (i = 0; i < it; i++) { // iteration for train
				s = st;
				eval = 1;
				randrow = myRo;
				randcol = myCo;
				RC = 0; // Route Scope
				route[3][100] = {};
				while ((randrow != opRo) || (randcol != (opCo - 1))){
					if (s <= 0)
						break;
					// ********************************************fill pk
					for (j = 0; j < 5; j++)
						pk[j] = DeMatrix[randrow][randcol][j];
					if ((randrow) == opRo && (randcol - 1) == opCo)
						pk[3] = p;
					// ********************************************neighbors check
					if ((randrow - 1) == opRo && (randcol) == opCo)
						pk[0] = p;
					if ((randrow + 1) == opRo && (randcol) == opCo)
						pk[2] = p;
					if ((randrow) == opRo && (randcol - 1) == opCo)
						pk[3] = p;
					if (randrow == opRo && (((randcol + 1) == opCo) || (randcol + 2) == opCo))
						pk[1] = p;
					// ********************************************normalization and exploration
					normal = 0;
					for (counter = 0; counter < 5 ; counter++){
						if (pk[counter] < 0.0001)
							pk[counter] = 0.0001;
						normal += pk[counter];
					}
					for (counter = 0; counter < 5 ; counter++)
						pk[counter] = pk[counter] / normal;
					// ******************************************** select random with probability
					ptemp[0] = pk[0]*32000;
					val = rand() % 32000;
					for (counter = 1; counter < 5 ; counter++)
						ptemp[counter] = ptemp[counter-1] + pk[counter] * 32000;
					for (counter = 0; counter < 5; counter++) {
						if (val < ptemp[counter]){
							action = counter;
							break;
						}
						val -= ptemp[counter];
					}
					// ******************************************** save route
					route[0][RC] = randrow;
					route[1][RC] = randcol;
					route[2][RC] = action;
					switch (action) {
					case 0: if (randrow != 0)
						randrow = randrow - 1;
						break;
					case 3: if (randcol != 0)
						randcol = randcol - 1;
						break;
					case 2: if (randrow != (6 - 1))
						randrow = randrow + 1;
						break;
					case 1: if (randcol != (9 - 1))
						randcol = randcol + 1;
						break;
					}    // end switch
					// ---------------Update
					s = s - 1;
					RC = RC + 1;
				}       // end while
				if ((randrow != opRo) || (randcol != (opCo - 1)))
					beta = 0;  //  fail
				else{
					beta = 1; // success
					eval = RC;
				}
				for (m = 0; m < RC; m++) {  // update actions in route with reward and penalty
					for (j = 0; j < 5; j++)
						pk[j] = DeMatrix[route[0][m]][route[1][m]][j];
					point_LReP2 = LReP(route[2][m], beta, 5, pk, landa1, landa2, eval);
					for (j = 0; j < 5; j++)
						Pr[j] = *(point_LReP2 + j);
					for (j = 0; j < 5; j++)
						DeMatrix[route[0][m]][route[1][m]][j] = Pr[j];
				} // end update probabilities for
			}// end iteration for

			// ******************************************** make action
			randrow = myRo;
			randcol = myCo;
			for (j = 0; j < 5; j++)
				pk[j] = DeMatrix[randrow][randcol][j];
			// Wall check*********************
			if (randrow == 0)
				pk[0] = pk[0] * (1 - p);
			if (randrow == 6 - 1)
				pk[2] = pk[2] * (1 - p);
			if (randcol == 9 - 1)
				pk[1] = pk[1] * (1 - p);
			if (randcol == 0)
				pk[3] = pk[3] * (1 - p);
			// ********************************************check neighbors
			if ((randrow) == opRo && (randcol - 1) == opCo)
				pk[3] = p;
			if ((randrow) == opRo && ((randcol + 1) == opCo || (randcol + 2) == opCo))
				pk[1] = p;
			// ********************************************normalization and exploration
			normal = 0;
			for (counter = 0; counter < 5 ; counter++){
				if (pk[counter] < 0.0001)
					pk[counter] = 0.0001;
				normal += pk[counter];
			}
			for (counter = 0; counter < 5 ; counter++)
				pk[counter] = pk[counter] / normal;

			// ********************************************
			ptemp[0] = pk[0] * 32000;
			val = rand() % 32000;
			for (counter = 1; counter < 5 ; counter++)
				ptemp[counter] = ptemp[counter-1] + pk[counter] * 32000;
			for (counter = 0; counter < 5; counter++) {
				if (val < ptemp[counter]){
					action = counter;
					break;
				}
				val -= ptemp[counter];
			}
			//********************************************
		} // end of else
		return action;
	}
