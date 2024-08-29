import React, { useEffect, useState } from 'react';
import { Node } from "../types/index"
import { 
	Table, Thead, Tbody, Tr, Th, Td, TableContainer, Box, Heading , Flex
} from '@chakra-ui/react';

const NodeList: React.FC = () => {
  const [nodes, setNodes] = useState<Node[]>([]);
	
	useEffect(() => {
    let env = process.env.REACT_APP_NODES || "";
		let nodes = env.split(',').map(node => { return {ip: node, status: "200", transaction_pool: []} });
		setNodes(nodes)
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
        boxShadow="sm"
        mt="10"
      >
        <Heading as="h2" size="lg" marginBottom="5" textAlign="center">
          Nodes
        </Heading>
        <TableContainer>
          <Table variant="simple">
            <Thead>
              <Tr bg="purple.600">
                <Th textAlign="center" color="white">IP</Th>
                <Th textAlign="center" color="white">Status</Th>
              </Tr>
            </Thead>
            <Tbody>
              {nodes.map((node, index) => (
                <Tr key={index}>
                  <Td textAlign="center">{node.ip}</Td>
                  <Td textAlign="center">{node.status}</Td>
                </Tr>
              ))}
            </Tbody>
          </Table>
        </TableContainer>
      </Box>
    </Flex>
	);
};

export default NodeList;

