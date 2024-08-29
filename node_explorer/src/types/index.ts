export type Transaction = {
  hash: string;
  sender: string;
  receiver: string;
  amount: number;
	gas_limit: number;
	gas_price: number;
	nonce: number;
	block: number;
	payload: string;
  timestamp: string;
}

export type Block = {
	index: number;
	previous_hash: string;
	timestamp: string;
	transactions: Transaction[];
	miner: string;
	nonce: number;
	difficulty: number;
	hash: string;
}

export type Node = {
  ip: string;
  status: string;
  transaction_pool: Transaction[]
}
