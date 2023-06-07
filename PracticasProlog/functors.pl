evento(siglo15,"Portugueses y españoles exploran África, América y Asia."). 
evento(siglo16,"Leo Da Vinci pinta la Mona Lisa.").
evento(siglo17,"Construcción del Taj Majal."). 
evento(siglo18,"Ben Framklin inventa lentes bifocales."). 
evento(siglo19,"Independencia de México.").
evento(siglo20, "Invención de Internet."). 
evento(siglo21, "Caída de Torres gemelas."). 
antes_de(evento(siglo15,_), evento(siglo16,_)). 
antes_de(evento(siglo16,_), evento(siglo17,_)).
antes_de(evento(siglo17,_), evento(siglo18,_)). 
antes_de(evento(siglo18,_), evento(siglo19,_)).
antes_de(evento(siglo19,_), evento(siglo20,_)). 
antes_de(evento(siglo20,_), evento(siglo21,_)). 

antes_de(X,Y):-
	antes_de(X,Z), 
	antes_de(Z,Y). 

