// import React from 'react';
// import logo from './logo.svg';
// import './App.css';

import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import Header from './components/Header';
import Dashboard from './components/Dashboard';
import NodeList from './components/NodeList';
import TransactionList from './components/TransactionList';
import { ChakraProvider } from '@chakra-ui/react'

function App() {
  return (
  <ChakraProvider>
    <Router>
      <Header />
      <Routes>
        <Route path="/" element={<Dashboard/>} />
        <Route path="/nodes" element={<NodeList/>} />
        <Route path="/transactions" element={<TransactionList/>} />
      </Routes>
    </Router>
  </ChakraProvider>
  );
}

export default App;
