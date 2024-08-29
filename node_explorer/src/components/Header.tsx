import React from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { Box, Flex, Link, Heading } from '@chakra-ui/react';

const Header: React.FC = () => {
  return (
    <Box bg="purple.600" padding="4">
      <Flex 
        alignItems="center" 
        justifyContent="space-between" 
        maxW="1200px" 
        margin="0 auto"
      >
        <Heading as="h1" size="md" color="white">
          <Link as={RouterLink} to="/" _hover={{ textDecoration: 'none' }}>
            Agora Explorer
          </Link>
        </Heading>
        <nav>
          <Flex gap="4">
            <Link 
              as={RouterLink} 
              to="/" 
              color="white" 
              _hover={{ textDecoration: 'underline' }}
            >
              Dashboard
            </Link>
            <Link 
              as={RouterLink} 
              to="/nodes" 
              color="white" 
              _hover={{ textDecoration: 'underline' }}
            >
              Nodes
            </Link>
            <Link 
              as={RouterLink} 
              to="/transactions" 
              color="white" 
              _hover={{ textDecoration: 'underline' }}
            >
              Transactions
            </Link>
          </Flex>
        </nav>
      </Flex>
    </Box>
  );
};

export default Header;

