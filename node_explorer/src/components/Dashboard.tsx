import React from 'react';
import { Box, SimpleGrid, GridItem, Text, Heading, Flex, HStack, Icon } from '@chakra-ui/react';
import { LineChart, Line, XAxis, YAxis, Tooltip } from 'recharts';
import { FaCubes, FaReceipt } from 'react-icons/fa';

const Dashboard: React.FC = () => {
  // Sample data for charts
  const latestBlocks = [
    { id: '20636116', miner: 'beaverbuild', transactions: 152, time: '11 secs ago', reward: '0.05071' },
    { id: '20636115', miner: 'Titan Builder', transactions: 211, time: '23 secs ago', reward: '0.03514' },
    { id: '20636116', miner: 'beaverbuild', transactions: 152, time: '11 secs ago', reward: '0.05071' },
    { id: '20636116', miner: 'beaverbuild', transactions: 152, time: '11 secs ago', reward: '0.05071' },
    { id: '20636115', miner: 'Titan Builder', transactions: 211, time: '23 secs ago', reward: '0.03514' },
    { id: '20636115', miner: 'Titan Builder', transactions: 211, time: '23 secs ago', reward: '0.03514' },
  ];

  const latestTransactions = [
    { hash: '0x7d3b6d74e2...', from: '0x95222290...', to: '0x8cfb1220...', amount: '0.09822' },
    { hash: '0x7d3b6d74e2...', from: '0x95222290...', to: '0x8cfb1220...', amount: '0.09822' },
    { hash: '0x7d3b6d74e2...', from: '0x95222290...', to: '0x8cfb1220...', amount: '0.09822' },
    { hash: '0x29ee8017b...', from: '0xb280FCfA...', to: '0x49Fd6A61...', amount: '1.57564' },
    { hash: '0x29ee8017b...', from: '0xb280FCfA...', to: '0x49Fd6A61...', amount: '1.57564' },
    { hash: '0x29ee8017b...', from: '0xb280FCfA...', to: '0x49Fd6A61...', amount: '1.57564' },
  ];

  return (
    <Box padding="6" bg="gray.50" minHeight="100vh">
      <SimpleGrid columns={{ base: 1, md: 3 }} spacing={6} mb={6}>
        <Box bg="white" p="4" borderRadius="lg" boxShadow="md">
          <Text fontSize="lg" fontWeight="bold">Nodes</Text>
          <Text fontSize="2xl">3</Text>
        </Box>
        <Box bg="white" p="4" borderRadius="lg" boxShadow="md">
          <Text fontSize="lg" fontWeight="bold">Blocks</Text>
          <Text fontSize="2xl">2</Text>
        </Box>
        <Box bg="white" p="4" borderRadius="lg" boxShadow="md">
          <Text fontSize="lg" fontWeight="bold">Transactions</Text>
          <Text fontSize="2xl">12</Text>
        </Box>
      </SimpleGrid>

      <SimpleGrid columns={{ base: 1, lg: 2 }} spacing={6}>

        <GridItem bg="white" p="4" borderRadius="lg" boxShadow="md">
          <Flex justify="space-between" align="center" mb="4">
            <Heading size="md">Latest Blocks</Heading>
          </Flex>
          {latestBlocks.map((block, index) => (
            <HStack key={index} mb="3">
              <Icon as={FaCubes} boxSize={5} />
              <Box>
                <Text fontWeight="bold">{block.id}</Text>
                <Text fontSize="sm">{block.miner} • {block.transactions} txns in {block.time}</Text>
              </Box>
              <Text ml="auto">{block.reward}</Text>
            </HStack>
          ))}
        </GridItem>

        <GridItem bg="white" p="4" borderRadius="lg" boxShadow="md">
          <Flex justify="space-between" align="center" mb="4">
            <Heading size="md">Latest Transactions</Heading>
          </Flex>
          {latestTransactions.map((tx, index) => (
            <HStack key={index} mb="3">
              <Icon as={FaReceipt} boxSize={5} />
              <Box>
                <Text fontWeight="bold">{tx.hash}</Text>
                <Text fontSize="sm">From {tx.from} to {tx.to}</Text>
              </Box>
              <Text ml="auto">{tx.amount}</Text>
            </HStack>
          ))}
        </GridItem>

      </SimpleGrid>
    </Box>
  );
};

export default Dashboard;
