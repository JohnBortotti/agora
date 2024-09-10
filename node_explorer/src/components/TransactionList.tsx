import React, { useState, useEffect } from 'react';
import { Transaction } from "../types/index"
import axios from 'axios';
import { 
	Table, Thead, Tbody, Tr, Th, Td, TableContainer, Box, Heading , Flex, Select
} from '@chakra-ui/react';

const TransactionList: React.FC = () => {
  const [nodes, setNodes] = useState<string[]>([]);
  const [selectedNode, setSelectedNode] = useState<string>('');
  const [transactions, setTransactions] = useState<Transaction[]>([]);

	const handleRowClick = (hash: string) => {
		console.log(`Transaction clicked: ${hash}`);
	};

	const getTransactions = (node: string) => {
    axios.get(`${node}/chain`) 
      .then(response => {

				const blocks = response.data;

				const transactions = blocks.reduce((acc: any[], block: any) => {
					const txs = block.transactions.map((tx: any) => {
						return {
							hash: tx.hash,
							sender: tx.sender,
							receiver: tx.receiver,
							amount: tx.amount,
							gas_limit: tx.gas_limit,
							gas_price: tx.gas_price,
							nonce: tx.nonce,
							block: block.index,
							payload: tx.payload,
							timestamp: block.timestamp
						};
					});
					return acc.concat(txs);
				}, []);

				setTransactions([...transactions])
			})
      .catch(error => console.error('Error fetching transactions:', error));
	}

	const truncateString = (str='', maxLength=10): String => {
	  return str.length > maxLength 
    ? `${str.substring(0, maxLength)}…`
    : str
	}

	const handleNodeChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
		setSelectedNode(event.target.value);
		getTransactions(event.target.value);
	};

  useEffect(() => {
		let env = process.env.REACT_APP_NODES || "";
		let nodes = env.split(',');
		setNodes(nodes);
		setSelectedNode(nodes[0])
    getTransactions(nodes[0])
  }, []);

  return (
       <Flex 
      direction="column" 
      alignItems="center" 
      justifyContent="flex-start" 
      bg="gray.50"
			width="100%"
      minHeight="100vh"
    >
      <Box 
        padding="5" 
        maxW="6xl" 
        width="100%" 
        borderWidth="1px" 
        borderRadius="lg" 
        bg="white"
        boxShadow="lg"
        mt="10"
      >
        <Heading as="h2" size="lg" marginBottom="5" textAlign="center">
          Transactions
        </Heading>
				
				<Flex justifyContent="flex-end">
					<Select
						value={selectedNode}
						onChange={handleNodeChange}
						maxW="200px"
						textAlign="center"
						lineHeight="2.7"
						mb="5"
					>
						{nodes.map((node) => (
							<option key={node} value={node}>
								{node}
							</option>
						))}
					</Select>
				</Flex>

        <TableContainer>
          <Table variant="simple">
            <Thead>
              <Tr bg="purple.600">
                <Th color="white">Hash</Th>
                <Th color="white">Sender</Th>
                <Th color="white">Receiver</Th>
                <Th color="white">Amount</Th>
                <Th color="white">Gas limit</Th>
                <Th color="white">Gas price</Th>
                <Th color="white">Nonce</Th>
                <Th color="white">Block</Th>
                <Th color="white">Payload</Th>
                <Th color="white">Timestamp</Th>
              </Tr>
            </Thead>
            <Tbody>
              {transactions.map(tx => (
                <Tr 
                  key={tx.hash}
                  onClick={() => handleRowClick(tx.hash)}
                  cursor="pointer"
                  _hover={{ bg: "gray.100" }}
                >
                  <Td>{truncateString(tx.hash)}</Td>
                  <Td>{truncateString(tx.sender)}</Td>
                  <Td>{truncateString(tx.receiver)}</Td>
                  <Td color="green.500" fontWeight="bold">{tx.amount}</Td>
                  <Td>{tx.gas_limit}</Td>
                  <Td>{tx.gas_price}</Td>
                  <Td>{tx.nonce}</Td>
                  <Td>{tx.block}</Td>
                  <Td>{tx.payload}</Td>
                  <Td>{tx.timestamp}</Td>
                </Tr>
              ))}
            </Tbody>
          </Table>
        </TableContainer>
      </Box>
    </Flex>
  );
};

export default TransactionList;

