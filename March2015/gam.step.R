library(gam)
gam.fit = gam(Survived~.-PassengerId, 
              data=raw_data,
              family=binomial(link="logit"))

g.scope = gam.scope(raw_data, response=2,
                  smoother="s",
                  arg="df=10",
                  form=TRUE)

#require(doMC)
#registerDoMC(cores=2)
gam.step.fit = step.gam(gam.fit, scope=g.scope, 
                        steps=1000,
                        direction="forward",
                        trace=2)

