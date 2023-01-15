require(rstan)

n_cores = 16

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

rstan_options(threads_per_chain = as.integer(n_cores/4))

dataset <- commandArgs(trailingOnly=TRUE)[1]

level <- commandArgs(trailingOnly=TRUE)[2]
  
modtype <- commandArgs(trailingOnly=TRUE)[3]

data.list <- readRDS(paste(dataset,'_',level,'_data.RDS',sep=''))

model_basic_hier = "functions {
  matrix fill_matrix(vector rates, int J) {
    matrix[J,J] Q = rep_matrix(0,J,J);
    //1 = absent, 2 = nonbasic_noIDCC, 3 = nonbasic_IDCC, 4 = basic_noIDCC, 5 = basic_IDCC
    Q[1,2] = 1e-10; //absent to nonbasic_noIDCC
    Q[1,3] = 1e-10; //absent to nonbasic_IDCC
    Q[1,4] = 1e-10; //absent to basic_noIDCC
    Q[1,5] = 1e-10; //absent to basic_IDCC
    Q[2,1] = rates[1]; //nonbasic_noIDCC to absent
    Q[2,3] = rates[2]; //nonbasic_noIDCC to nonbasic_IDCC
    Q[2,4] = rates[3]; //nonbasic_noIDCC to basic_noIDCC
    //nonbasic_noIDCC to basic_IDCC
    Q[3,1] = rates[4]; //nonbasic_IDCC to absent
    Q[3,2] = rates[5]; //nonbasic_IDCC to nonbasic_noIDCC
    //nonbasic_IDCC to basic_noIDCC
    Q[3,5] = rates[6]; //nonbasic_IDCC to basic_IDCC
    //basic_noIDCC to absent
    Q[4,2] = rates[7]; //basic_noIDCC to nonbasic_noIDCC
    //basic_noIDCC to nonbasic_IDCC
    Q[4,5] = rates[8]; //basic_noIDCC to basic_IDCC
    //basic_IDCC to absent
    //basic_IDCC to nonbasic_noIDCC
    Q[5,3] = rates[9]; //basic_IDCC to nonbasic_IDCC
    Q[5,4] = rates[10]; //basic_IDCC to basic_noIDCC
    for (j in 1:J) {
      Q[j,j] = -sum(Q[j,]);
    }
    return(Q);
  }
  vector stat_prob(matrix Q, int J) {
    vector[J] pi = rep_vector(0,J);
    if (J > 3) {
      pi[2:J] = to_vector(matrix_exp((Q[2:J,2:J] + diag_matrix(Q[2:J,1]))*10000)[1,]);
    }
    else {
      pi[2] = Q[3,2]/(Q[3,2]+Q[2,3]);
      pi[3] = Q[2,3]/(Q[3,2]+Q[2,3]);
    }
    return(pi);
  }
  //compute likelihood via Felsenstein's Pruning Algorithm
  vector pruning( vector beta , vector theta , data real[] xr , data int[] xi ) {
    int N = xi[1];
    int B = xi[2];
    int J = xi[3];
    int tiplik_[N*J] = xi[4:(N*J+3)];
    int child[B] = xi[(N*J+4):(N*J+3+B)];
    int parent[B] = xi[(N*J+4+B):(N*J+3+2*B)];
    real brlen[B] = xr[1:B];
    real sites[B] = xr[(B+1):2*B];
    matrix[J,J] Q = fill_matrix(beta .* theta,J);
    vector[J] pi = stat_prob(Q, J);
    real mu;
    mu = dot_product(pi,Q[,1]);
    real log_lik;
    vector[B] phi = log(rep_vector(0,B));
    matrix[N,J] lambda;
    for (j in 1:J) {
      lambda[,j] = to_vector(log(tiplik_[((N*j)-(N-1)):(N*j)]));
    }
    for (b in 1:B) {
      matrix[J,J] P = matrix_exp(Q*brlen[b]); //via matrix exponentiation
        for (j in 1:J) {
          lambda[parent[b],j] += log_sum_exp(log(P[j])+lambda[child[b],]);
        }
        phi[b] = log(sites[b]) + log_sum_exp(log(pi) + to_vector(lambda[child[b],]) + log(1-exp(-mu*brlen[b])));
    }
    log_lik = log_sum_exp(phi);
    return([log_lik]');
  }
}
data {
  int<lower=1> D;
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1> B;
  int xi[D,3+N*J+2*B];                  //likelihoods for data at tips+internal nodes in of each tree
  real xr[D,2*B];
  real brlen[B];
}
transformed data {
  int xi_[D,3+N*J+2*B];
  real xr_[D,2*B];
  for (d in 1:D) {
    xi_[d] = to_array_1d(xi[d]);
    xr_[d] = to_array_1d(xr[d]);
  }
}
parameters {
  real<lower=0> alpha;
  real<lower=0> delta[D];
  vector<lower=0>[J*2] beta;
  vector<lower=0>[J*2] theta[D];
}
model {
  alpha ~ exponential(1.5);
  delta ~ exponential(1.5);
  beta ~ gamma(alpha,alpha);
  for (d in 1:D) {
    theta[d] ~ gamma(delta[d],delta[d]);
  }
  target += sum(map_rect(pruning,beta,theta,xr_,xi_));
}"

model_basic_flat = "functions {
  matrix fill_matrix(vector rates, int J) {
    matrix[J,J] Q = rep_matrix(0,J,J);
    //1 = absent, 2 = nonbasic_noIDCC, 3 = nonbasic_IDCC, 4 = basic_noIDCC, 5 = basic_IDCC
    Q[1,2] = 1e-10; //absent to nonbasic_noIDCC
    Q[1,3] = 1e-10; //absent to nonbasic_IDCC
    Q[1,4] = 1e-10; //absent to basic_noIDCC
    Q[1,5] = 1e-10; //absent to basic_IDCC
    Q[2,1] = rates[1]; //nonbasic_noIDCC to absent
    Q[2,3] = rates[2]; //nonbasic_noIDCC to nonbasic_IDCC
    Q[2,4] = rates[3]; //nonbasic_noIDCC to basic_noIDCC
    //nonbasic_noIDCC to basic_IDCC
    Q[3,1] = rates[4]; //nonbasic_IDCC to absent
    Q[3,2] = rates[5]; //nonbasic_IDCC to nonbasic_noIDCC
    //nonbasic_IDCC to basic_noIDCC
    Q[3,5] = rates[6]; //nonbasic_IDCC to basic_IDCC
    //basic_noIDCC to absent
    Q[4,2] = rates[7]; //basic_noIDCC to nonbasic_noIDCC
    //basic_noIDCC to nonbasic_IDCC
    Q[4,5] = rates[8]; //basic_noIDCC to basic_IDCC
    //basic_IDCC to absent
    //basic_IDCC to nonbasic_noIDCC
    Q[5,3] = rates[9]; //basic_IDCC to nonbasic_IDCC
    Q[5,4] = rates[10]; //basic_IDCC to basic_noIDCC
    for (j in 1:J) {
      Q[j,j] = -sum(Q[j,]);
    }
    return(Q);
  }
  vector stat_prob(matrix Q, int J) {
    vector[J] pi = rep_vector(0,J);
    if (J > 3) {
      pi[2:J] = to_vector(matrix_exp((Q[2:J,2:J] + diag_matrix(Q[2:J,1]))*10000)[1,]);
    }
    else {
      pi[2] = Q[3,2]/(Q[3,2]+Q[2,3]);
      pi[3] = Q[2,3]/(Q[3,2]+Q[2,3]);
    }
    return(pi);
  }
  //compute likelihood via Felsenstein's Pruning Algorithm
  vector pruning( vector beta , vector theta , data real[] xr , data int[] xi ) {
    int N = xi[1];
    int B = xi[2];
    int J = xi[3];
    int tiplik_[N*J] = xi[4:(N*J+3)];
    int child[B] = xi[(N*J+4):(N*J+3+B)];
    int parent[B] = xi[(N*J+4+B):(N*J+3+2*B)];
    real brlen[B] = xr[1:B];
    real sites[B] = xr[(B+1):2*B];
    matrix[J,J] Q = fill_matrix(beta,J);
    vector[J] pi = stat_prob(Q, J);
    real mu;
    mu = dot_product(pi,Q[,1]);
    real log_lik;
    vector[B] phi = log(rep_vector(0,B));
    matrix[N,J] lambda;
    for (j in 1:J) {
      lambda[,j] = to_vector(log(tiplik_[((N*j)-(N-1)):(N*j)]));
    }
    for (b in 1:B) {
      matrix[J,J] P = matrix_exp(Q*brlen[b]); //via matrix exponentiation
        for (j in 1:J) {
          lambda[parent[b],j] += log_sum_exp(log(P[j])+lambda[child[b],]);
        }
        phi[b] = log(sites[b]) + log_sum_exp(log(pi) + to_vector(lambda[child[b],]) + log(1-exp(-mu*brlen[b])));
    }
    log_lik = log_sum_exp(phi);
    return([log_lik]');
  }
}
data {
  int<lower=1> D;
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1> B;
  int xi[D,3+N*J+2*B];                  //likelihoods for data at tips+internal nodes in of each tree
  real xr[D,2*B];
  real brlen[B];
}
transformed data {
  int xi_[D,3+N*J+2*B];
  real xr_[D,2*B];
  vector[0] theta[D];
  for (d in 1:D) {
    xi_[d] = to_array_1d(xi[d]);
    xr_[d] = to_array_1d(xr[d]);
  }
}
parameters {
  real<lower=0> alpha;
  vector<lower=0>[J*2] beta;
}
model {
  alpha ~ exponential(1.5);
  beta ~ gamma(alpha,alpha);
  target += sum(map_rect(pruning,beta,theta,xr_,xi_));
}"

model_present_hier = "functions {
  matrix fill_matrix(vector rates, int J) {
    matrix[J,J] Q = rep_matrix(0,J,J);
    //1 = absent, 2 = no IDCC, 3 = ID lab, ...
    Q[1,2:J] = to_row_vector(rep_vector(1e-10,J-1));
    {int k=1; for (i in 2:J) { for (j in 1:J) {
        if (i != j) {
          Q[i,j] = rates[k]; k += 1;
        }
      }
    }    }
    for (j in 1:J) {
      Q[j,j] = -sum(Q[j,]);
    }
    return(Q);
  }
  vector stat_prob(matrix Q, int J) {
    vector[J] pi = rep_vector(0,J);
    if (J > 3) {
      pi[2:J] = to_vector(matrix_exp((Q[2:J,2:J] + diag_matrix(Q[2:J,1]))*10000)[1,]);
    }
    else {
      pi[2] = Q[3,2]/(Q[3,2]+Q[2,3]);
      pi[3] = Q[2,3]/(Q[3,2]+Q[2,3]);
    }
    return(pi);
  }
  vector pruning( vector beta , vector theta , data real[] xr , data int[] xi ) {
    int N = xi[1];
    int B = xi[2];
    int J = xi[3];
    int tiplik_[N*J] = xi[4:(N*J+3)];
    int child[B] = xi[(N*J+4):(N*J+3+B)];
    int parent[B] = xi[(N*J+4+B):(N*J+3+2*B)];
    real brlen[B] = xr[1:B];
    real sites[B] = xr[(B+1):2*B];
    matrix[J,J] Q = fill_matrix(beta .* theta,J);
    vector[J] pi = stat_prob(Q, J);
    real mu;
    mu = dot_product(pi,Q[,1]);
    real log_lik;
    vector[B] phi = log(rep_vector(0,B));
    matrix[N,J] lambda;
    for (j in 1:J) {
      lambda[,j] = to_vector(log(tiplik_[((N*j)-(N-1)):(N*j)]));
    }
    for (b in 1:B) {
      matrix[J,J] P = matrix_exp(Q*brlen[b]); //via matrix exponentiation
        for (j in 1:J) {
          lambda[parent[b],j] += log_sum_exp(log(P[j])+lambda[child[b],]);
        }
        phi[b] = log(sites[b]) + log_sum_exp(log(pi) + to_vector(lambda[child[b],]) + log(1-exp(-mu*brlen[b])));
    }
    log_lik = log_sum_exp(phi);
    return([log_lik]');
  }
}
data {
  int<lower=1> D;
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1> B;
  int xi[D,3+N*J+2*B];                  //likelihoods for data at tips+internal nodes in of each tree
  real xr[D,2*B];
  real brlen[B];
}
transformed data {
  int xi_[D,3+N*J+2*B];
  real xr_[D,2*B];
  for (d in 1:D) {
    xi_[d] = to_array_1d(xi[d]);
    xr_[d] = to_array_1d(xr[d]);
  }
}
parameters {
  real<lower=0> alpha;
  real<lower=0> delta[D];
  vector<lower=0>[(J-1)*(J-1)] beta;
  vector<lower=0>[(J-1)*(J-1)] theta[D];
}
model {
  alpha ~ exponential(1.5);
  delta ~ exponential(1.5);
  beta ~ gamma(alpha,alpha);
  for (d in 1:D) {
    theta[d] ~ gamma(delta[d],delta[d]);
  }
  target += sum(map_rect(pruning,beta,theta,xr_,xi_));
}"

model_present_flat = "functions {
  matrix fill_matrix(vector rates, int J) {
    matrix[J,J] Q = rep_matrix(0,J,J);
    //1 = absent, 2 = no IDCC, 3 = ID lab, ...
    Q[1,2:J] = to_row_vector(rep_vector(1e-10,J-1));
    {int k=1; for (i in 2:J) { for (j in 1:J) {
        if (i != j) {
          Q[i,j] = rates[k]; k += 1;
        }
      }
    }    }
    for (j in 1:J) {
      Q[j,j] = -sum(Q[j,]);
    }
    return(Q);
  }
  vector stat_prob(matrix Q, int J) {
    vector[J] pi = rep_vector(0,J);
    if (J > 3) {
      pi[2:J] = to_vector(matrix_exp((Q[2:J,2:J] + diag_matrix(Q[2:J,1]))*10000)[1,]);
    }
    else {
      pi[2] = Q[3,2]/(Q[3,2]+Q[2,3]);
      pi[3] = Q[2,3]/(Q[3,2]+Q[2,3]);
    }
    return(pi);
  }
  vector pruning( vector beta , vector theta , data real[] xr , data int[] xi ) {
    int N = xi[1];
    int B = xi[2];
    int J = xi[3];
    int tiplik_[N*J] = xi[4:(N*J+3)];
    int child[B] = xi[(N*J+4):(N*J+3+B)];
    int parent[B] = xi[(N*J+4+B):(N*J+3+2*B)];
    real brlen[B] = xr[1:B];
    real sites[B] = xr[(B+1):2*B];
    matrix[J,J] Q = fill_matrix(beta,J);
    vector[J] pi = stat_prob(Q, J);
    real mu;
    mu = dot_product(pi,Q[,1]);
    real log_lik;
    vector[B] phi = log(rep_vector(0,B));
    matrix[N,J] lambda;
    for (j in 1:J) {
      lambda[,j] = to_vector(log(tiplik_[((N*j)-(N-1)):(N*j)]));
    }
    for (b in 1:B) {
      matrix[J,J] P = matrix_exp(Q*brlen[b]); //via matrix exponentiation
        for (j in 1:J) {
          lambda[parent[b],j] += log_sum_exp(log(P[j])+lambda[child[b],]);
        }
        phi[b] = log(sites[b]) + log_sum_exp(log(pi) + to_vector(lambda[child[b],]) + log(1-exp(-mu*brlen[b])));
    }
    log_lik = log_sum_exp(phi);
    return([log_lik]');
  }
}
data {
  int<lower=1> D;
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1> B;
  int xi[D,3+N*J+2*B];                  //likelihoods for data at tips+internal nodes in of each tree
  real xr[D,2*B];
  real brlen[B];
}
transformed data {
  int xi_[D,3+N*J+2*B];
  real xr_[D,2*B];
  vector[0] theta[D];
  for (d in 1:D) {
    xi_[d] = to_array_1d(xi[d]);
    xr_[d] = to_array_1d(xr[d]);
  }
}
parameters {
  real<lower=0> alpha;
  vector<lower=0>[(J-1)*(J-1)] beta;
}
model {
  alpha ~ exponential(1.5);
  beta ~ gamma(alpha,alpha);
  target += sum(map_rect(pruning,beta,theta,xr_,xi_));
}"

model_code = ''

if (dataset == 'basic' & modtype == 'hier') {
  model_code = model_basic_hier
}
if (dataset == 'basic' & modtype == 'flat') {
  model_code = model_basic_flat
}
if (dataset == 'present' & modtype == 'hier') {
  model_code = model_present_hier
}
if (dataset == 'present' & modtype == 'flat') {
  model_code = model_present_flat
}

fit <- stan(model_code=model_code,data=data.list,cores=n_cores)

save.image(paste('../output/',dataset,'_',level,'_',modtype,'.Rdata',sep=''))